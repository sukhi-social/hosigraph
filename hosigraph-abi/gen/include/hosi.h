/* hosi.h -- written by setu from the description of the hosi API. Do not edit. */
#ifndef HOSI_H
#define HOSI_H

/* The contract version. It changes when this header, or what an operation takes or gives,
   changes in a way an older caller cannot follow. */
#define HOSI_ABI_VERSION 1

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct hosi_conn hosi_conn;
typedef struct hosi_opts hosi_opts;   /* the library's options; NULL means the defaults */
typedef uint64_t hosi_id;

/* How a call ended. The numbers are part of the ABI: OK is 0 and the others follow in this order. */
typedef enum {
  HOSI_OK = 0,
  HOSI_ERR_APP,       /* hosigraph refused the request: a write it would not keep, a query it could not run, a file that is corrupt */
  HOSI_ERR_PROTOCOL,  /* a breach of the contract: a request that is not UTF-8, not EDN or not the shape the operation takes, or a broken wire protocol */
  HOSI_ERR_IO,        /* reading or writing failed */
  HOSI_ERR_TIMEOUT,   /* the request ran out of time */
  HOSI_ERR_CANCELLED, /* cancelled by hosi_cancel */
  HOSI_ERR_CLOSED,    /* the connection, or the runtime, was closed before the answer */
  HOSI_ERR_INTERNAL   /* something unexpected inside the library */
} hosi_status;

/* Called exactly once for each submit that returned 0, on a thread of the library, never on
   the thread that called submit, cancel or close. Keep it short: no blocking, no exception,
   no longjmp, and do not call close from it. buf is valid only during the call (NULL when n is 0). */
typedef void (*hosi_done)(hosi_id id, hosi_status st,
                          const uint8_t *buf, size_t n, void *ud);

/* Once per process. 0 on success (also when it is already running). A start after a shutdown is
   refused (-EPERM). Starts the Haskell runtime without taking over the process's signals. */
int  hosi_runtime_init(void);
/* Only when the process is ending: closes every connection, then stops the runtime.
   Calls that start after it are refused. */
void hosi_runtime_shutdown(void);

/* Returns a handle at once; getting ready goes on in the background. uri is UTF-8 text and
   what it means is the library's to say. NULL opts means the defaults. A failure to get ready
   is reported to every request that waited for it. NULL if the runtime is not running. */
hosi_conn *hosi_open(const char *uri, const hosi_opts *opts);
/* Every request still waiting is answered with CLOSED. Returns after every callback has returned,
   so what ud points to may be freed after it. */
void       hosi_close(hosi_conn *c);

/* Returns 0 if the request is accepted; then done is called exactly once.
   Otherwise done is never called, and the result is a negative errno: -EINVAL (the id is already
   in use, or c is not an open connection), -EBUSY (too many requests are waiting), -EPIPE (the
   connection or the runtime is closed). The body is UTF-8 EDN and the ABI does not read it;
   it is copied before submit returns. Requests start in the order of submit and may finish
   in any order. */
int hosi_submit(hosi_conn *c, hosi_id id,
                const uint8_t *req, size_t n,
                hosi_done cb, void *ud);

/* May be called any number of times (idempotent). If the cancel and the finish meet,
   exactly one of them is reported. Returns 0, or -EINVAL if c is not an open connection. */
int hosi_cancel(hosi_conn *c, hosi_id id);

/* A thread that is done with the library may say so (optional). */
void hosi_thread_done(void);

/* The contract version this library was built with (the same number as HOSI_ABI_VERSION). */
uint32_t hosi_abi_version(void);

#ifdef __cplusplus
}
#endif

#endif /* HOSI_H */
