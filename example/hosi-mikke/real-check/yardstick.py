# SPDX-License-Identifier: BlueOak-1.0.0
# Step 2 of the check on real posts (README.md, "On real posts"): the yardstick, and the queries.
#
#   HOSIGRAPH_SUDACHI=<sudachi> HOSIGRAPH_SUDACHI_DICT=<system_core.dic> python3 -I yardstick.py <work-dir>
#
# Reads <work-dir>/notes.json (extract.py), cuts every note with sudachi itself (mode A, and C for the compound nouns), and works out for
# queries (dictionary forms of content words, and long compound nouns that mode A cuts into several words) which notes "have the word":
# the notes where the query's words come in a row, allowing inflection and spelling.  That is set by Sudachi, so it is a yardstick
# that favours Sudachi, not the truth.  Prints counts and WORDS only, never a post; writes <work-dir>/yard.json and queries.txt.
import json, re, subprocess, sys, time, collections, statistics, threading, os
if len(sys.argv) != 2: sys.exit(__doc__)
WORK = sys.argv[1]
EXE = os.environ['HOSIGRAPH_SUDACHI']
DIC = os.environ['HOSIGRAPH_SUDACHI_DICT']
CHUNK = 3000
notes = json.load(open(WORK + '/notes.json', encoding='utf-8'))
texts = [t for _, t in notes]
N = len(texts)
flat = lambda t: t.replace('\n', ' ').replace('\r', ' ').replace('\t', ' ')

def run_sudachi(mode, lines):
    p = subprocess.Popen([EXE, '-l', DIC, '-m', mode, '-a', '--split-sentences', 'no'],
                         stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    threading.Thread(target=lambda: (p.stdin.write(('\n'.join(lines) + '\n').encode()), p.stdin.close()), daemon=True).start()
    out, cur = [], []
    for raw in p.stdout:
        l = raw.decode('utf-8', 'replace').rstrip('\n').rstrip('\r')
        if l == 'EOS': out.append(cur); cur = []
        else: cur.append(l.split('\t'))
    p.wait()
    assert len(out) == len(lines)
    return out

lines, owner = [], []
for i, t in enumerate(texts):
    f = flat(t)
    cs = [f[k:k + CHUNK] for k in range(0, len(f), CHUNK)] or ['']
    for c in cs: lines.append(c); owner.append(i)

SKIP = {'助詞', '助動詞', '補助記号', '空白'}
ascii_ = lambda s: all(ord(c) < 128 for c in s)
def words_of(an):
    out = []
    for f in an:
        if len(f) < 3: continue
        surf, pos, norm = f[0], f[1].split(',')[0], f[2]
        if not surf.strip() or pos in SKIP: continue
        w = surf.lower() if ascii_(surf) else (norm or surf).lower()
        if any(c.isalnum() for c in w): out.append(w)      # the rule of wordsOfAnalysis: a word has a letter or a digit
    return out

A = run_sudachi('A', lines)
words = [[] for _ in texts]
dicts = collections.defaultdict(set)      # typed string (dictionary form) -> docs where a content token has it
for an, i in zip(A, owner):
    words[i] += words_of(an)
    for f in an:
        if len(f) < 4: continue
        p = f[1].split(',')
        pos1, pos2 = p[0], (p[1] if len(p) > 1 else '')
        ok = (pos1 == '名詞' and pos2 not in ('代名詞', '数詞')) or (pos1 in ('動詞', '形容詞') and pos2 != '非自立可能')
        d = f[3]
        if ok and d and not ascii_(d) and not re.fullmatch(r'[0-9０-９]+', d): dicts[d].add(i)

CJK = r'[㐀-䶿一-鿿\U00020000-\U0002ffff぀-ゟ゠-ヿ가-힯ー]'
RUN = re.compile(CJK + '+|[^\\W_]+'); CJK0 = re.compile('^' + CJK)
def pair_terms(s):
    out = []
    for r in RUN.findall(s.lower()):
        out += ([r] if len(r) < 2 else [r[k:k + 2] for k in range(len(r) - 1)]) if CJK0.match(r) else [r]
    return out
pair_idx = collections.defaultdict(set); word_idx = collections.defaultdict(set)
for i, t in enumerate(texts):
    for w in set(pair_terms(t)): pair_idx[w].add(i)
    for w in set(words[i]): word_idx[w].add(i)
lowers = [t.lower() for t in texts]

def phrase_docs(ws):
    """docs whose word stream has ws as consecutive words"""
    if not ws: return set()
    cand = set.intersection(*[word_idx[w] for w in ws]) if all(w in word_idx for w in ws) else set()
    if len(ws) == 1: return cand
    n = len(ws); out = set()
    for i in cand:
        d = words[i]
        if any(d[k:k + n] == ws for k in range(len(d) - n + 1)): out.add(i)
    return out

def evaluate(Qs, label):
    # tokenize the queries the way the pipeline does
    W = [words_of(an) for an in run_sudachi('A', Qs)]
    rows = collections.defaultdict(list)
    for q, ws in zip(Qs, W):
        if not ws: continue
        ph = phrase_docs(ws)
        if not ph: continue
        S_or  = set.union(*[word_idx.get(w, set()) for w in ws])
        S_and = set.intersection(*[word_idx.get(w, set()) for w in ws])
        ts = pair_terms(q)
        Pand = set.intersection(*[pair_idx.get(t, set()) for t in ts]) if ts else set()
        Por  = set.union(*[pair_idx.get(t, set()) for t in ts]) if ts else set()
        L = '1' if len(q) == 1 else ('2' if len(q) == 2 else '3+')
        rows[L].append((q, ws, ph, S_or, S_and, Pand, Por))
    print(f'\n== {label}: {sum(len(v) for v in rows.values())} queries with a hit')
    print('  (relevant = notes where the query\'s words come in a row, allowing inflection and spelling; set by Sudachi, so read as a yardstick, not as truth)')
    print(f'  {"len":>4} {"n":>4} | {"phrase":>7} | {"sudachi AND":>21} | {"sudachi OR":>21} | {"pairs AND":>21} | {"pairs OR":>21}')
    print(f'  {"":>4} {"":>4} | {"found":>7} | {"recall":>9} {"prec":>9} | {"recall":>9} {"prec":>9} | {"recall":>9} {"prec":>9} | {"recall":>9} {"prec":>9}')
    def agg(items):
        tp = collections.Counter(); got = collections.Counter(); rel = 0
        for q, ws, ph, So, Sa, Pa, Po in items:
            rel += len(ph)
            for name, s in (('sa', Sa), ('so', So), ('pa', Pa), ('po', Po)):
                tp[name] += len(s & ph); got[name] += len(s)
        return rel, tp, got
    for L in ('1', '2', '3+', 'all'):
        items = [x for k, v in rows.items() for x in v] if L == 'all' else rows.get(L, [])
        if not items: continue
        rel, tp, got = agg(items)
        f = lambda n: f'{tp[n]/rel:9.3f} {tp[n]/max(1,got[n]):9.3f}'
        print(f'  {L:>4} {len(items):>4} | {rel:>7} | {f("sa")} | {f("so")} | {f("pa")} | {f("po")}')
    return rows

# 1. content words, as typed (dictionary form)
Q1 = [d for d, s in sorted(dicts.items(), key=lambda kv: -len(kv[1])) if len(s) >= 5][:400]
rows1 = evaluate(Q1, f'content words, dictionary form (df {len(dicts[Q1[0]])}..{len(dicts[Q1[-1]])})')

# why do pairs miss / add, by example words (words only)
def examples(rows, key, n=18):
    miss = collections.Counter(); extra = collections.Counter()
    for L, items in rows.items():
        for q, ws, ph, So, Sa, Pa, Po in items:
            m = len(ph - Pa); e = len(Pa - ph)
            if m: miss[(q, '|'.join(ws))] = m
            if e: extra[(q, '|'.join(ws))] = e
    print(f'\n  most notes pairs (AND) do not find but the word is there:   [typed | words] count')
    for (q, w), c in miss.most_common(n): print(f'    {c:4d}  {q} | {w}')
    print(f'\n  most notes pairs (AND) find but the word is not there as such:')
    for (q, w), c in extra.most_common(n): print(f'    {c:4d}  {q} | {w}')
examples(rows1, 'content')

# 2. long compound nouns (mode C units that mode A cuts again)
C = run_sudachi('C', lines)
cdocs = collections.defaultdict(set)
for an, i in zip(C, owner):
    for f in an:
        if len(f) < 4: continue
        p = f[1].split(',')
        if p[0] == '名詞' and (p[1] if len(p) > 1 else '') in ('普通名詞', '固有名詞') and len(f[0]) >= 3 and not ascii_(f[0]): cdocs[f[0]].add(i)
Qc = [d for d, s in sorted(cdocs.items(), key=lambda kv: -len(kv[1])) if len(s) >= 3]
# keep the ones A cuts into 2+ words
cutA = run_sudachi('A', Qc); Qc = [q for q, an in zip(Qc, cutA) if len(words_of(an)) >= 2][:300]
rows2 = evaluate(Qc, f'compound nouns that A cuts into several words ({len(Qc)} queries)')
print('\n  examples (typed -> words), words only:')
for L, items in rows2.items():
    for q, ws, ph, So, Sa, Pa, Po in items[:0]: pass
sample = [x for v in rows2.values() for x in v][:12]
for q, ws, ph, So, Sa, Pa, Po in sample:
    print(f'    {q} -> {"|".join(ws)}   phrase {len(ph)}, sudachi AND {len(Sa)}, OR {len(So)}, pairs AND {len(Pa)}, OR {len(Po)}')


# --- the answers, for the end-to-end check
dump = {}
for name, rows in (('content', rows1), ('compound', rows2)):
    for L, items in rows.items():
        for q, ws, ph, So, Sa, Pa, Po in items:
            dump[q] = {'kind': name, 'ws': ws, 'phrase': sorted(ph), 'sa': sorted(Sa), 'so': sorted(So), 'pa': sorted(Pa), 'po': sorted(Po)}
json.dump(dump, open(WORK + '/yard.json', 'w', encoding='utf-8'), ensure_ascii=False)
with open(WORK + '/queries.txt', 'w', encoding='utf-8') as f:
    for q, d in dump.items(): f.write(q + '\t' + str(max(1, len(d['phrase']))) + '\n')
print('\nwrote', len(dump), 'queries')
