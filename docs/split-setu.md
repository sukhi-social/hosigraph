# Moving `setu/` to a repository of its own

This is a plan to read and carry out later, not something that has been done. `setu/` is built to be lifted out:
nothing in it names hosigraph (only its README points at `hosigraph-abi` as an example), and its one dependency
that is not on Hackage, `takka`, is already taken by name. This page says what was made ready, what was tried,
and what is still for the owner to decide.

## What is ready

| In `setu/` | What it is for |
|---|---|
| `setu.cabal` | has its own description, `tested-with`, `extra-doc-files`; `takka >=0.1.0.1 && <0.2`; no hosigraph in it |
| `README.md`, `CHANGELOG.md` | what it is, what has and has not been seen working |
| `cabal.project.standalone` | how it builds on its own (takka from git); renamed `cabal.project` at the split. Its `branch: setu` works once this work is pushed: the branch on the server still has takka 0.1.0.0, and setu asks for 0.1.0.1 |
| `.github/workflows/ci.yml` | the test job of the new repository (GitHub reads it only after the split, where it is at the top) |
| `.gitignore` | `dist-newstyle/`; without it `git add -A` in the new repository picks up the build directory |

While `setu/` is in this repository none of these changes anything: cabal takes the nearest `cabal.project`, and
`cabal.project.standalone` has another name; GitHub reads only the `.github` at the top.

## What was tried (a rehearsal in a scratch clone, 2026-11-12)

1. `git subtree split --prefix=setu -b setu-only` gave a history of only the commits that touched `setu/`.
2. A new repository from that branch, with `cabal.project.standalone` renamed to `cabal.project` and `takka` pinned by `tag: <hash>`:
   `cabal build --enable-tests && cabal test` gave **138 passed, 0 failed**, takka fetched from git.
3. The hosigraph side with `setu/` removed, and setu taken from the new repository by `source-repository-package`:
   `hosigraph-abi-gen --check hosigraph-abi/gen` ok, `hosigraph-abi:spec` 32 passed.
4. `cabal sdist` holds the golden header, the README and the changelog. `cabal check` says what is still missing (below).

What was not tried: pushing anywhere, GitHub Actions in the new repository, Linux.

## Steps

Do these in order. Nothing before step 3 leaves your machine.

1. **Commit the work on the `setu` branch first.** The split reads commits, not the working tree.
2. **Split.** In this repository:
   ```bash
   git subtree split --prefix=setu -b setu-only
   ```
   The hashes of the commits change (the paths change), so the old ones are not in the new repository.
3. **Make the new repository, empty** (no README or licence from the site), and push the branch as its `main`:
   ```bash
   git push <url-of-the-new-repository> setu-only:main
   ```
4. **In the new repository**, one commit:
   * `git mv cabal.project.standalone cabal.project`
   * in it, replace `branch: setu` with `tag: <hash>`: a commit of hosigraph's **main** that holds takka 0.1.0.1 or later.
     A branch moves, and a hash that lived only on a branch that was squash-merged is gone.
     So merge the takka change into main first, and pin that.
   * in `setu.cabal`: `license` and a `LICENSE` file, `maintainer`, `homepage`, `bug-reports`, and
     ```
     source-repository head
       type:     git
       location: <url-of-the-new-repository>
     ```
     (`cabal check` asks for these three; Hackage rejects a package without a licence.)
   * the link in `README.md` to `hosigraph-abi` can stay.
   * `ci.yml` needs no change.
5. **In hosigraph, one pull request** that does the other half, so that main is never without setu:
   * `git rm -r setu`
   * `cabal.project`: take `setu` out of `packages:` and add
     ```
     source-repository-package
       type: git
       location: <url-of-the-new-repository>
       tag: <hash of its main>
     ```
   * `.github/workflows/test.yml`: line 75 drops `setu` from `cabal build`; line 79 drops `setu:spec` (it is not a local package any more);
     lines 53 and 105 drop `'setu/setu.cabal'` from `hashFiles`; the comment at line 8 changes.
   * `README.md`, "From other languages": `setu/` becomes a link.
   * `hosigraph-abi/hosigraph-abi.cabal`: give `setu` a bound (`setu ^>=0.0.1`), now that it is released apart.
6. **Check** in that pull request: `cabal run hosigraph-abi:exe:hosigraph-abi-gen -- --check hosigraph-abi/gen`,
   `cabal test hosigraph:spec hosigraph-abi:spec`, and the `abi` job.

To rehearse again without touching anything real, the same steps were run in a scratch clone with `file://` URLs in place of the two repository URLs.

## What the owner has to decide (not decided here)

* **The licence.** There is no LICENSE file anywhere in this repository, and no author or maintainer in any `.cabal` file. The new repository cannot be
  used by anyone else until this is chosen, and hosigraph's own is the same question.
* **The names.** `setu` and `hosigraph-abi` are working names. A rename is cheaper before the split than after.
* **T-13 and T-14.** The two rules added to the original twelve (a callback is never on the thread that called; `close` returns after the last callback)
  are kept by `Setu.Ticket` and tried by its tests; the README says they are only the author's proposal. Nobody else has agreed to them, and a rule that
  an outside user may rely on should be agreed before the first release.
* **takka.** setu reads and writes EDN through `Takka.Edn`, and takka lives in hosigraph's repository, so after the split each repository points at the other
  (setu at hosigraph for takka, hosigraph at setu for the doors). It works, but it is a loop. Ways out: publish takka (Hackage, or its own repository),
  or take the EDN part out of takka into a small package of its own. Neither has been started.
* **Whether to split at all yet.** While hosigraph is the only customer, the directory costs nothing, and a second repository costs a release and a pin
  for every change that crosses. The preparation above makes the split a short job, so it can wait for a second customer.
