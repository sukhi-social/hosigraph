# SPDX-License-Identifier: BlueOak-1.0.0
# Step 4 of the check on real posts (README.md, "On real posts"): score the answers of hosigraph against the yardstick.
#
#   python3 -I score.py <work-dir> [out.tsv] [R|all|N]
#
# <work-dir>/yard.json (yardstick.py), idmap.tsv and out.tsv (mikke-real-check).  Each query was asked with {:top T} (T = R, the number
# of notes that have the word in a row, unless mikke-real-check was given another: say it here, as the last argument): a search of several words gives the notes that have all of them
# among the best T by BM25.  R-precision is what was found of the R notes that have the word in a row; precision is what of the notes
# returned is one of them; "lack a word" must be 0 (a note returned that is not in the set that has every word).  Prints numbers only.
import json, collections, sys, os
if len(sys.argv) not in (2, 3, 4): sys.exit(__doc__)
WORK = sys.argv[1]
out = sys.argv[2] if len(sys.argv) > 2 else WORK + '/out.tsv'
TOP = sys.argv[3] if len(sys.argv) > 3 else 'R'        # the :top that mikke-real-check was given
yard = json.load(open(WORK + '/yard.json', encoding='utf-8'))
idx_of = {}
for l in open(WORK + '/idmap.tsv'):
    i, e = l.split('\t'); idx_of[int(e)] = int(i)       # entity id -> note index

res = {}
for l in open(out, encoding='utf-8'):
    l = l.rstrip('\n')
    if not l: continue
    q, r, ids = (l.split('\t') + ['', ''])[:3]
    res[q] = (int(r), [idx_of[int(x)] for x in ids.split() if int(x) in idx_of])

rows = collections.defaultdict(list)
for q, (R, got) in res.items():
    d = yard.get(q)
    if not d: continue
    ph = set(d['phrase']); g = set(got)
    kind = d['kind']; L = '1' if len(q) == 1 else ('2' if len(q) == 2 else '3+')
    cap = R if TOP == 'R' else (10 ** 9 if TOP == 'all' else int(TOP))
    rows[kind].append(dict(q=q, R=R, cap=cap, got=len(g), hit=len(g & ph), nws=len(d['ws']), so=len(d['so']), sa=len(d['sa']), ph=len(ph), L=L, lack=len(g - set(d['sa']))))

print(f'{"kind":>9} {"n":>4} | {"R-precision (top R = number of relevant notes)":>46} | {"queries with top R fully right":>30}')
for kind, items in rows.items():
    for sel, lab in ((lambda x: True, 'all'), (lambda x: x['nws'] >= 2, '2+ words'), (lambda x: x['nws'] == 1, '1 word')):
        it = [x for x in items if sel(x)]
        if not it: continue
        rp = sum(x['hit'] / x['R'] for x in it) / len(it)
        full = sum(1 for x in it if x['hit'] == x['R'])
        print(f'{kind:>9} {len(it):>4} {lab:>9} | macro R-precision {rp:6.3f}   (notes found {sum(x["hit"] for x in it)} of {sum(x["R"] for x in it)}) | {full} of {len(it)}')
# what was returned, against the sets of the yardstick
for kind, items in rows.items():
    for sel, lab in ((lambda x: x['nws'] >= 2, '2+ words'), (lambda x: x['nws'] == 1, '1 word')):
        it = [x for x in items if sel(x)]
        if not it: continue
        got = sum(x['got'] for x in it)
        prec = sum((x['hit'] / x['got']) if x['got'] else 1.0 for x in it) / len(it)
        print(f'{kind:>9} {lab:>9} | notes returned {got}; macro precision {prec:6.3f}; queries that returned nothing {sum(1 for x in it if x["got"] == 0)}; '
              f'notes that lack a word {sum(x["lack"] for x in it)} (must be 0 for 2+ words); queries that returned fewer than min(:top, AND-set) {sum(1 for x in it if x["got"] < min(x["cap"], x["sa"]))}')
