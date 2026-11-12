#include <stdint.h>
#include <stddef.h>

typedef struct dtlv_conn dtlv_conn;
typedef struct dtlv_opts dtlv_opts;  // 依頼のタイムアウト、同時件数の上限、プール数
typedef uint64_t dtlv_id;

typedef enum {
  DTLV_OK = 0,
  DTLV_ERR_SERVER,    // サーバーがエラーを返した(buf に EDN)
  DTLV_ERR_PROTOCOL,  // 通信の約束違反
  DTLV_ERR_IO,        // 接続・読み書きの失敗
  DTLV_ERR_TIMEOUT,
  DTLV_ERR_CANCELLED,
  DTLV_ERR_CLOSED,
  DTLV_ERR_INTERNAL   // Haskell 側の想定外
} dtlv_status;

typedef void (*dtlv_done)(dtlv_id id, dtlv_status st,
                          const uint8_t *buf, size_t n, void *ud);

// プロセスで 1 回だけ
int  dtlv_runtime_init(void);
void dtlv_runtime_shutdown(void);   // プロセス終了時のみ

// 接続は非同期に確立し、すぐにハンドルを返す
dtlv_conn *dtlv_open(const char *uri, const dtlv_opts *opts);
void       dtlv_close(dtlv_conn *c);

// 0 なら受理。コールバックはちょうど 1 回
int dtlv_submit(dtlv_conn *c, dtlv_id id,
                const uint8_t *req, size_t n,
                dtlv_done cb, void *ud);
int dtlv_cancel(dtlv_conn *c, dtlv_id id);   // 冪等

// 外部スレッドが使い終わったら呼ぶ(任意)
void dtlv_thread_done(void);
