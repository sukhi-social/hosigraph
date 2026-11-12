# TODO

## Phase 2, the S3 part — against RustFS

**Decided (2026-11-12, nyanrus):** the object store we build and test against is **RustFS**
([github.com/rustfs/rustfs](https://github.com/rustfs/rustfs); Apache-2.0, S3-compatible, written in Rust).
**Not decided:** how hosigraph talks to it (see item 1).

### What we know about RustFS (read from its docs on 2026-11-12; nothing has been run here yet)

* S3 API on port **9000**, web console on **9001**.
* **Path-style** addressing is the default: `http://host:9000/<bucket>/<key>`. Virtual-hosted style needs
  `RUSTFS_SERVER_DOMAINS`, wildcard DNS and a TLS certificate. We use path-style only.
* **AWS Signature V4.** Region is `RUSTFS_REGION`, default `us-east-1`.
* The docs list put, get, head, copy, delete, multi-delete, `ListObjects`/`ListObjectsV2`, multipart, versioning, and "range reads".
* The docs **do not say** how range reads behave (syntax, multi-range, edge cases). We only ever send one range, `Range: bytes=a-b`;
  the spike below has to confirm it.
* The docs say RustFS is **not identical to AWS S3 or MinIO** and tell you to validate the release you use.
* Credentials: one source says `rustfsadmin` / `rustfsadmin`; set `RUSTFS_ACCESS_KEY` and `RUSTFS_SECRET_KEY` ourselves in every test.
  Never write a key in the manifest, the repository or the README.
* Docker: image `rustfs/rustfs`, runs as UID/GID 10001, so a bind-mounted data directory must be owned by 10001.
  Pin a version tag in tests, not `latest`.
* On this Mac: `podman` is at `/opt/podman/bin/podman` (whether a machine is running is not checked); there is no `docker` and no `rustfs` binary.
  `curl` is 8.7.1 and knows `--aws-sigv4`.

### 0. Spike first (small, nothing committed)

Start RustFS (podman), make a bucket, then try by hand and write down what really happens:

* PUT a file; GET the whole file; HEAD for the size.
* GET with `Range: bytes=100-199`: is it `206`, is `Content-Range` right, are the bytes exact? A range past the end? `bytes=0-0`?
* GET a key that is not there (status, body); wrong secret key (status, body); a bucket that is not there.
* `ListObjectsV2` with a prefix (needed later for cleaning up chunks that are not in the manifest).
* With `curl --aws-sigv4 "aws:amz:us-east-1:s3" --user ...`: does a PUT need `x-amz-content-sha256` set by hand (e.g. `UNSIGNED-PAYLOAD`) or does curl add it?
* How long one range GET takes on localhost (only to have a floor; it says nothing about a real network).

The answers decide items 1 and 4. Put them in `docs/design-notes.md` when known.

### 1. Open decision: the transport

| | what | cost |
|---|---|---|
| **A. `curl` subprocess** (the leaning) | `--aws-sigv4` signs, `-r` asks for a range; one process per request | no new Haskell package, no C; needs `curl` on the machine; ~ms of process start for each request |
| B. HTTP library + our own SigV4 | one connection kept open | today the package needs only `aeson` besides GHC's own libraries and has no C; an HTTP + TLS stack usually brings a large tree of packages (the crypto ones often with C inside), and our own signing code to test |
| C. plain HTTP by hand over `network` | same | the same signing code as B, and no TLS (fine for localhost, not for a real deployment) |

Whatever we choose, it sits behind the interface in item 2, so it can change later.

### 2. An object-store interface (`Hosigraph.ObjectStore`)

A small record, like `ChunkSource`: `put key bytes`, `getRange key offset len`, `getWhole key`, `headSize key`, `list prefix`.
(`delete` waits for Phase 3.) Errors keep three things apart: **the object is not there**, **the store cannot be reached**, **we are not allowed**.

### 3. A fake bucket for tests (no network)

A directory-backed (or in-memory) store with the same interface that **counts requests and bytes**.
Everything below is first tested on it, and it always runs. This is where "a question about the past skips the chunks it does not need" is proved with numbers.

### 4. The RustFS client

Path-style, SigV4, endpoint / region / bucket / key prefix in `HosiConfig`; credentials from the environment or the caller, never stored.
Single PUT first; add multipart only when a chunk is big enough to need it (the limits are not in RustFS's docs: find them in the spike).

### 5. Upload

After `seal`: put `chunk.hosi` and `filter.bin` (not `mphf.idx`: the block table is enough on the deep path, and it was bigger than the data in our measurement).
Order: **data first, manifest last**, so a crash leaves an object that nobody points at (like a chunk that is not in the manifest today).
Compare the size and sha256 with the manifest before the manifest says "remote".
**Question for nyanrus:** after a verified upload, does the local copy stay (a cache) or go (remote only)? Phase 3's freezing probably decides; for now keep it.

### 6. Manifest: where a chunk lives

A chunk entry gains a place (local, or remote with bucket and key prefix). Old manifests must still read as "local".
Check how the manifest hash and signature cover it. Cleaning of objects the manifest does not list: `list prefix`, then delete (after Phase 3 has `delete`).

### 7. Events picks a source for each chunk

`sourceFor :: ChunkEntry -> ChunkSource`. A remote chunk's filter is one whole GET (~11 KB for 4,000 ids). Open question: keep filters on local disk
so that a restart does not fetch all of them again? Also the known gap: filters are cached with no limit.

### 8. When the store is unreachable

`history` must **fail**, never return a shorter list that looks complete. Add a `HosiError` for it. Timeouts. Retry with backoff for 5xx and connection errors;
no retry for 403 and 404. Test this with the fake bucket (it can be told to fail).

### 9. `verify` for remote chunks

Cheap by default (size and `headSize`), a deep option that downloads and checks the sha256 and the filter.

### 10. A test against a real RustFS (gated by environment, like `HOSIGRAPH_DTLV`)

For example `HOSIGRAPH_S3_ENDPOINT`, `HOSIGRAPH_S3_BUCKET`, `HOSIGRAPH_S3_ACCESS_KEY`, `HOSIGRAPH_S3_SECRET_KEY`. It checks: upload then read back, the range reads give
exact bytes, open = 4 ranges, one lookup = 1 range, a missing object and a wrong key give the right error. In CI: a `rustfs/rustfs:<pinned tag>` service container on ubuntu-24.04
(mind UID 10001). Locally: podman.

### 11. Measure and write it down

Open and lookup against RustFS on localhost, with the request counts from item 3. Localhost is a floor, not a forecast: say so in the README.

### 12. Docs

README: how to configure the store; how to start RustFS for the tests. `docs/design-notes.md`: what the spike found, and where RustFS differs from the S3 the design document pictures.

### Not now (Phase 3 and later)

* Erase and bucket versioning: the design document says an Erase is complete only after the old versions are deleted too (line 123 of the design document). Check what RustFS does with versioning then.
* Freezing / cold chunks, garbage collection of remote chunks, lifecycle rules.
* Expanding RustFS: a single-node single-drive deployment cannot be grown in place (its docs say so). Not our problem for tests; it matters for a real deployment.
