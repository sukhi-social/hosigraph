# hosi (Elixir)

hosigraph from Elixir, through `libhosi` (the C ABI that setu wrote, `hosigraph-abi/gen/include/hosi.h`).
A small C NIF (`c_src/hosi_nif.c`) turns each `hosi_done` callback into a message; a GenServer per
connection numbers the requests and gives each answer to its caller. Bodies are EDN (`Hosi.Edn`).

```elixir
{:ok, db} = Hosi.start_link(open: %{dir: "/var/lib/hosi", schema: %{"person/name": %{type: :string}}})
{:ok, report} = Hosi.write(db, [[:create, [:tmp, 1], :"person/name", "Frege"]])
Hosi.stop(db)
```

## Getting libhosi

`mix compile` puts the NIF and `libhosi` into `priv/` one of two ways (`support/hosi_build.ex`):

* **Prebuilt** (the default on a machine with no source): it downloads the bundle for this machine
  (`aarch64-darwin`, `x86_64-linux`, `aarch64-linux`, and for Alpine `x86_64-musl`, `aarch64-musl`; OTP 26
  or newer; the glibc ones need glibc 2.39 or newer, as they are built on Ubuntu 24.04) from the GitHub release `hosi-ex-vX.Y.Z`, checks its sha256 against
  `checksum.txt` in this package, and unpacks it. No Haskell, no C compiler. A checkout with no
  `checksum.txt` entry for its version does not download anything.
* **From source**: needs a C compiler and `libhosi` (`cabal build hosigraph-abi:flib:hosi`). It is used
  when `libhosi` is found in this repository's `dist-newstyle`, or at `HOSI_LIB_DIR`:

  ```sh
  HOSI_LIB_DIR=/dir/with/libhosi HOSI_INCLUDE_DIR=/dir/with/hosi.h mix test
  ```

`HOSI_BUILD=source|precompiled|none` says which (none: use `priv/` as it is);
`HOSI_PRECOMPILED_URL` points at a mirror of the release's files.

## Releasing (the prebuilt bundles)

1. Set `@version` in `mix.exs`; tag `hosi-ex-vX.Y.Z`; push the tag.
2. Workflow `precompile-hosi-ex` builds a bundle on each machine, runs the Elixir tests on it with the
   source tree out of reach, and puts the tarballs and `checksum.txt` on the release.
3. Commit that `checksum.txt` into `hosigraph-ex/` and use that commit (or publish to Hex).
   A tag cannot hold its own checksums, so the commit that is used comes after the tag.

The tests use a connection with no database (`open: %{}`): ping, abi, the error kinds, many callers
at once, stop and kill. **Not tried here:** a real database (`:dir`), which needs `dtlv` or the
Datalevin jar.

## Things to know

* The runtime starts once per process and is never shut down from Elixir (`hosi_runtime_shutdown`
  is "when the process is ending"). Connections are closed by `Hosi.stop/1` or when their process ends.
* Keywords become atoms (schema names are the user's own, and few). `##NaN` etc. read as `{:float, :nan}`.
* `hosi_cancel` is in the NIF but not offered in `Hosi`: the ids are the connection's own.
* A bundle is big (about 75 MB unpacked, 16 MB packed on macOS): `hosigraph` depends on `aeson`, which brings
  its own dependencies along as shared libraries.
* The macOS bundle and the musl one for aarch64 have run (the musl one by `scripts/ci-musl-bundle.sh` in `alpine:3.20`, with
  the Alpine GHC that ghcup picks, dynamically linked: 18 tests passed on the unpacked bundle). The glibc bundles and
  x86_64-musl have not yet; the workflow is their first run.
* musl's `ldd` loads a library alone, so it prints `Error relocating ... symbol not found` for every RTS (or `enif_*`) symbol
  that only the host provides. It is noise; the scripts hide it.
