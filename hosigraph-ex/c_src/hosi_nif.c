/* hosi_nif.c -- the Erlang side of libhosi (hosi.h).
 *
 * Thin on purpose.  The library calls `done` on a thread of its own, so the answer leaves as a
 * message to the process that submitted: {:hosi_done, id, status, body}.  All the thinking
 * (ids, who waits for what, EDN) is in Elixir.
 */
#include <erl_nif.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "hosi.h"

static ErlNifResourceType *CONN;

typedef struct { hosi_conn *c; } conn_res;
typedef struct { ErlNifPid pid; } waiter;

static ERL_NIF_TERM A_ok, A_error, A_hosi_done, A_closed, A_runtime, A_einval, A_ebusy, A_epipe, A_eperm,
                    A_errno;
static ERL_NIF_TERM STATUS[8];   /* indexed by hosi_status */

static ERL_NIF_TERM err(ErlNifEnv *env, ERL_NIF_TERM why) {
  return enif_make_tuple2(env, A_error, why);
}

static void conn_dtor(ErlNifEnv *env, void *obj) {
  (void)env;
  conn_res *r = obj;
  if (r->c) { hosi_close(r->c); r->c = NULL; }   /* a conn nobody closed: answer what waits, then go */
}

static int load(ErlNifEnv *env, void **priv, ERL_NIF_TERM info) {
  (void)priv; (void)info;
  if (hosi_abi_version() != HOSI_ABI_VERSION) return 1;   /* header and library disagree */
  CONN = enif_open_resource_type(env, NULL, "hosi_conn", conn_dtor, ERL_NIF_RT_CREATE, NULL);
  if (!CONN) return 1;
#define ATOM(v, s) v = enif_make_atom(env, s)
  ATOM(A_ok, "ok"); ATOM(A_error, "error"); ATOM(A_hosi_done, "hosi_done");
  ATOM(A_closed, "closed"); ATOM(A_runtime, "runtime"); ATOM(A_einval, "einval");
  ATOM(A_ebusy, "ebusy"); ATOM(A_epipe, "epipe"); ATOM(A_eperm, "eperm");
  ATOM(A_errno, "errno");
  ATOM(STATUS[HOSI_OK], "ok");               ATOM(STATUS[HOSI_ERR_APP], "app");
  ATOM(STATUS[HOSI_ERR_PROTOCOL], "protocol"); ATOM(STATUS[HOSI_ERR_IO], "io");
  ATOM(STATUS[HOSI_ERR_TIMEOUT], "timeout"); ATOM(STATUS[HOSI_ERR_CANCELLED], "cancelled");
  ATOM(STATUS[HOSI_ERR_CLOSED], "closed");   ATOM(STATUS[HOSI_ERR_INTERNAL], "internal");
#undef ATOM
  return 0;
}

/* init() -> :ok | {:error, :eperm | {:errno, n}} ; idempotent. */
static ERL_NIF_TERM n_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  (void)argc; (void)argv;
  int rc = hosi_runtime_init();
  if (rc == 0) return A_ok;
  if (rc == -EPERM) return err(env, A_eperm);
  return err(env, enif_make_tuple2(env, A_errno, enif_make_int(env, rc)));
}

/* open(uri :: binary) -> {:ok, conn} | {:error, :runtime} */
static ERL_NIF_TERM n_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  (void)argc;
  ErlNifBinary uri;
  if (!enif_inspect_binary(env, argv[0], &uri)) return enif_make_badarg(env);
  char *z = malloc(uri.size + 1);
  if (!z) return enif_raise_exception(env, enif_make_atom(env, "enomem"));
  memcpy(z, uri.data, uri.size);
  z[uri.size] = 0;
  hosi_conn *c = hosi_open(z, NULL);
  free(z);
  if (!c) return err(env, A_runtime);
  conn_res *r = enif_alloc_resource(CONN, sizeof *r);
  r->c = c;
  ERL_NIF_TERM t = enif_make_resource(env, r);
  enif_release_resource(r);
  return enif_make_tuple2(env, A_ok, t);
}

/* Runs on a thread of the library. */
static void on_done(hosi_id id, hosi_status st, const uint8_t *buf, size_t n, void *ud) {
  waiter *w = ud;
  ErlNifEnv *e = enif_alloc_env();
  ERL_NIF_TERM bin;
  unsigned char *p = enif_make_new_binary(e, n, &bin);
  if (n) memcpy(p, buf, n);
  ERL_NIF_TERM s = ((unsigned)st < 8) ? STATUS[st] : STATUS[HOSI_ERR_INTERNAL];
  ERL_NIF_TERM msg = enif_make_tuple4(e, A_hosi_done, enif_make_uint64(e, id), s, bin);
  enif_send(NULL, &w->pid, e, msg);   /* a dead receiver is not our trouble */
  enif_free_env(e);
  free(w);
}

/* submit(conn, id :: non_neg_integer, body :: binary, receiver :: pid)
 *   -> :ok | {:error, :einval | :ebusy | :epipe | :closed} */
static ERL_NIF_TERM n_submit(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  (void)argc;
  conn_res *r; ErlNifUInt64 id; ErlNifBinary body; ErlNifPid pid;
  if (!enif_get_resource(env, argv[0], CONN, (void **)&r) || !enif_get_uint64(env, argv[1], &id) ||
      !enif_inspect_binary(env, argv[2], &body) || !enif_get_local_pid(env, argv[3], &pid))
    return enif_make_badarg(env);
  if (!r->c) return err(env, A_closed);
  waiter *w = malloc(sizeof *w);
  if (!w) return enif_raise_exception(env, enif_make_atom(env, "enomem"));
  w->pid = pid;
  int rc = hosi_submit(r->c, id, body.data, body.size, on_done, w);
  if (rc == 0) return A_ok;
  free(w);   /* refused: on_done will never be called */
  switch (rc) {
    case -EINVAL: return err(env, A_einval);
    case -EBUSY:  return err(env, A_ebusy);
    case -EPIPE:  return err(env, A_epipe);
    default:      return err(env, enif_make_tuple2(env, A_errno, enif_make_int(env, rc)));
  }
}

/* cancel(conn, id) -> :ok | {:error, :einval | :closed} */
static ERL_NIF_TERM n_cancel(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  (void)argc;
  conn_res *r; ErlNifUInt64 id;
  if (!enif_get_resource(env, argv[0], CONN, (void **)&r) || !enif_get_uint64(env, argv[1], &id))
    return enif_make_badarg(env);
  if (!r->c) return err(env, A_closed);
  return hosi_cancel(r->c, id) == 0 ? A_ok : err(env, A_einval);
}

/* close(conn) -> :ok. Waits until every callback has returned, so it runs on a dirty scheduler. */
static ERL_NIF_TERM n_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  (void)argc;
  conn_res *r;
  if (!enif_get_resource(env, argv[0], CONN, (void **)&r)) return enif_make_badarg(env);
  hosi_conn *c = r->c;
  r->c = NULL;
  if (c) hosi_close(c);
  return A_ok;
}

static ERL_NIF_TERM n_abi_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  (void)argc; (void)argv;
  return enif_make_uint(env, hosi_abi_version());
}

static ErlNifFunc funcs[] = {
  {"init", 0, n_init, ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"open", 1, n_open, 0},
  {"submit", 4, n_submit, 0},
  {"cancel", 2, n_cancel, 0},
  {"close", 1, n_close, ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"abi_version", 0, n_abi_version, 0},
};

ERL_NIF_INIT(Elixir.Hosi.Nif, funcs, load, NULL, NULL, NULL)
