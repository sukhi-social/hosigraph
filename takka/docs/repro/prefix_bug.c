/* Minimal reproducer: dlmdb MDB_PREFIX_COMPRESSION loses a key.
 * Build: cc -I<dlmdb>/libraries/liblmdb prefix_bug.c mdb.o midl.o -lpthread
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dlmdb.h"
#define CK(x) do { int rc_ = (x); if (rc_) { printf("ERR %s: %s\n", #x, mdb_strerror(rc_)); exit(1);} } while (0)

static void put(MDB_txn *txn, MDB_dbi dbi, const unsigned char *k, size_t n) {
  MDB_val key = { n, (void *)k }, data = { 0, "" };
  CK(mdb_put(txn, dbi, &key, &data, 0));
}

int main(void) {
  system("rm -rf /tmp/repro_c/db; mkdir -p /tmp/repro_c/db");
  MDB_env *env; MDB_txn *txn; MDB_dbi dbi; MDB_cursor *cur;
  CK(mdb_env_create(&env)); CK(mdb_env_set_maxdbs(env, 4));
  CK(mdb_env_open(env, "/tmp/repro_c/db", 0, 0664));
  CK(mdb_txn_begin(env, NULL, 0, &txn));
  CK(mdb_dbi_open(txn, "t", MDB_CREATE | MDB_PREFIX_COMPRESSION, &dbi));   /* no MDB_COUNTED needed */
  const unsigned char k1[] = {2}, k2[] = {0, 2}, k3[] = {0, 1, 1};
  put(txn, dbi, k1, sizeof k1);   /* insert order matters */
  put(txn, dbi, k2, sizeof k2);
  put(txn, dbi, k3, sizeof k3);
  CK(mdb_txn_commit(env ? txn : txn));

  CK(mdb_txn_begin(env, NULL, MDB_RDONLY, &txn));
  CK(mdb_cursor_open(txn, dbi, &cur));
  MDB_val key, data; int rc;
  printf("expected scan: [0 1 1] [0 2] [2]\nactual scan:   ");
  for (rc = mdb_cursor_get(cur, &key, &data, MDB_FIRST); rc == 0; rc = mdb_cursor_get(cur, &key, &data, MDB_NEXT)) {
    printf("[");
    for (size_t i = 0; i < key.mv_size; i++) printf("%s%d", i ? " " : "", ((unsigned char *)key.mv_data)[i]);
    printf("] ");
  }
  printf("\n");
  MDB_val q = { sizeof k1, (void *)k1 };
  rc = mdb_get(txn, dbi, &q, &data);
  printf("mdb_get([2]) -> %s\n", rc == 0 ? "found" : mdb_strerror(rc));
  mdb_cursor_close(cur); mdb_txn_abort(txn); mdb_env_close(env);
  return 0;
}
