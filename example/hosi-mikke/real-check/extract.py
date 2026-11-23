# SPDX-License-Identifier: BlueOak-1.0.0
# Step 1 of the check on real posts (README.md, "On real posts").
#
#   python3 -I extract.py <datoms.edn> <work-dir>
#
# Takes the PUBLIC notes (as plain text) out of a Datalevin datoms dump of the sukhi fediverse server (lines like
# [123 :notes/content "<p>...</p>"] and [123 :notes/visibility "public"]) into <work-dir>/notes.json, a list of [entity, text].
# The work directory holds real posts: make it OUTSIDE this repository, and delete it when done.
import re, json, collections, html, os, sys
if len(sys.argv) != 3: sys.exit(__doc__)
SRC, WORK = os.path.expanduser(sys.argv[1]), os.path.expanduser(sys.argv[2])
os.makedirs(WORK, mode=0o700, exist_ok=True)
pat = re.compile(r'^\[(\d+) :notes/(content|visibility) (.*)\]$')
notes = collections.defaultdict(dict)
with open(SRC, encoding='utf-8') as f:
    for i, l in enumerate(f):
        if i < 1030: continue                       # in the dump of sukhi the first 1030 lines are the options and the schema
        m = pat.match(l.rstrip('\n'))
        if m:
            e = int(m.group(1)); k = m.group(2); v = m.group(3)
            if v.startswith('"'):
                try: v = json.loads(v)
                except Exception: v = None
            notes[e][k] = v
def plain(t):
    t = re.sub(r'<br\s*/?>', '\n', t); t = re.sub(r'</p>\s*<p>', '\n', t); t = re.sub(r'<[^>]+>', '', t); return html.unescape(t).strip()
P = [(e, plain(d['content'])) for e, d in notes.items() if d.get('visibility') == 'public' and isinstance(d.get('content'), str)]
P = [(e, p) for e, p in P if p]
fd = open(WORK + '/notes.json', 'w', encoding='utf-8'); json.dump(P, fd, ensure_ascii=False); fd.close()
os.chmod(WORK + '/notes.json', 0o600)
print('public non-empty notes:', len(P))
