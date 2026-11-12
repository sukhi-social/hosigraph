/* Minimal reproducer: mdb_count_range returns 0 as the first call in a fresh read txn
 * (per-DBI metadata is still DB_STALE); mdb_count_all refreshes it, mdb_count_range does not. */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "dlmdb.h"
#define CK(x) do { int rc_ = (x); if (rc_) { printf("ERR %s: %s\n", #x, mdb_strerror(rc_)); exit(1);} } while (0)
int main(void) {
  system("rm -rf /tmp/repro_c/db2; mkdir -p /tmp/repro_c/db2");
  MDB_env *env; MDB_txn *txn; MDB_dbi dbi;
  CK(mdb_env_create(&env)); CK(mdb_env_set_maxdbs(env, 4));
  CK(mdb_env_open(env, "/tmp/repro_c/db2", 0, 0664));
  CK(mdb_txn_begin(env, NULL, 0, &txn));
  CK(mdb_dbi_open(txn, "t", MDB_CREATE | MDB_COUNTED, &dbi));
  const char *keys[] = { "a", "b", "c", "d", "e" };
  for (int i = 0; i < 5; i++) { MDB_val k = { 1, (void *)keys[i] }, d = { 0, "" }; CK(mdb_put(txn, dbi, &k, &d, 0)); }
  CK(mdb_txn_commit(txn));

  MDB_val lo = { 1, "b" }, hi = { 1, "e" }; uint64_t n;
  CK(mdb_txn_begin(env, NULL, MDB_RDONLY, &txn));       /* fresh read txn */
  CK(mdb_count_range(txn, dbi, &lo, &hi, MDB_COUNT_LOWER_INCL, &n));
  printf("1st mdb_count_range([b,e)) = %llu   (expected 3)\n", (unsigned long long)n);
  CK(mdb_count_all(txn, dbi, 0, &n));
  printf("mdb_count_all             = %llu\n", (unsigned long long)n);
  CK(mdb_count_range(txn, dbi, &lo, &hi, MDB_COUNT_LOWER_INCL, &n));
  printf("2nd mdb_count_range([b,e)) = %llu   (expected 3)\n", (unsigned long long)n);
  mdb_txn_abort(txn); mdb_env_close(env); return 0;
}
