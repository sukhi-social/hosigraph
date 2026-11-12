# hosi (Elixir)

hosigraph from Elixir, through `libhosi` (the C ABI that setu wrote, `hosigraph-abi/gen/include/hosi.h`).
A small C NIF (`c_src/hosi_nif.c`) turns each `hosi_done` callback into a message; a GenServer per
connection numbers the requests and gives each answer to its caller. Bodies are EDN (`Hosi.Edn`).

```elixir
{:ok, db} = Hosi.start_link(open: %{dir: "/var/lib/hosi", schema: %{"person/name": %{type: :string}}})
{:ok, report} = Hosi.write(db, [[:create, [:tmp, 1], :"person/name", "Frege"]])
Hosi.stop(db)
```

## Build and test

Needs Elixir/OTP, a C compiler, and `libhosi` (`cabal build hosigraph-abi:hosi`). `mix` looks for
the library in this repository's `dist-newstyle`; otherwise say where:

```sh
HOSI_LIB_DIR=/dir/with/libhosi HOSI_INCLUDE_DIR=/dir/with/hosi.h mix test
```

The tests use a connection with no database (`open: %{}`): ping, abi, the error kinds, many callers
at once, stop and kill. **Not tried here:** a real database (`:dir`), which needs `dtlv` or the
Datalevin jar.

## Things to know

* The runtime starts once per process and is never shut down from Elixir (`hosi_runtime_shutdown`
  is "when the process is ending"). Connections are closed by `Hosi.stop/1` or when their process ends.
* Keywords become atoms (schema names are the user's own, and few). `##NaN` etc. read as `{:float, :nan}`.
* `hosi_cancel` is in the NIF but not offered in `Hosi`: the ids are the connection's own.
* On Linux and for a shipped package, `libhosi` must be found at run time (rpath is set to `HOSI_LIB_DIR`).
  Bundling it is undecided.
