/* hosi_abi.c -- written by setu from the description of the hosi API. Do not edit.
 *
 * The functions of hosi.h, around the Haskell ones (hosi_hs_*, in the generated Haskell module).
 * Two jobs: start the Haskell runtime, and let a call through only while it runs. */
#include <HsFFI.h>
#include <Rts.h>      /* hs_init_with_rtsopts is declared here, not in HsFFI.h */
#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#ifdef _WIN32
#  ifndef _WIN32_WINNT
#    define _WIN32_WINNT 0x0600   /* SRWLOCK and condition variables: Vista */
#  endif
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>
#else
#  include <pthread.h>
#endif
#include "hosi.h"

/* The Haskell half. The types are those of the Haskell signatures. */
extern uint64_t hosi_hs_open(const char *uri);
extern void     hosi_hs_close(uint64_t h);
extern int      hosi_hs_submit(uint64_t h, uint64_t id, const uint8_t *req, size_t n, hosi_done cb, void *ud);
extern int      hosi_hs_cancel(uint64_t h, uint64_t id);
extern void     hosi_hs_close_all(void);

/* ---- the gate ---------------------------------------------------------------- */

enum { NOT_STARTED, RUNNING, STOPPING, ENDED };

static int      g_state  = NOT_STARTED;
static unsigned g_inside = 0;          /* calls into Haskell in progress */

/* One lock and one condition ("nobody is inside"), whatever the system calls them. */
#ifdef _WIN32
static SRWLOCK            g_mu   = SRWLOCK_INIT;
static CONDITION_VARIABLE g_idle = CONDITION_VARIABLE_INIT;
static void gate_lock(void)   { AcquireSRWLockExclusive(&g_mu); }
static void gate_unlock(void) { ReleaseSRWLockExclusive(&g_mu); }
static void gate_wake(void)   { WakeAllConditionVariable(&g_idle); }
static void gate_wait(void)   { SleepConditionVariableSRW(&g_idle, &g_mu, INFINITE, 0); }   /* with the lock held */
#else
static pthread_mutex_t g_mu   = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t  g_idle = PTHREAD_COND_INITIALIZER;
static void gate_lock(void)   { pthread_mutex_lock(&g_mu); }
static void gate_unlock(void) { pthread_mutex_unlock(&g_mu); }
static void gate_wake(void)   { pthread_cond_broadcast(&g_idle); }
static void gate_wait(void)   { pthread_cond_wait(&g_idle, &g_mu); }   /* with the lock held */
#endif

static int enter(void) {
  int ok;
  gate_lock();
  ok = (g_state == RUNNING);
  if (ok) g_inside++;
  gate_unlock();
  return ok;
}

static void leave(void) {
  gate_lock();
  if (--g_inside == 0) gate_wake();
  gate_unlock();
}

/* ---- the runtime ------------------------------------------------------------- */

int hosi_runtime_init(void) {
  int rc = 0;
  gate_lock();
  if (g_state == STOPPING || g_state == ENDED) {
    rc = -EPERM;            /* not hs_init: the RTS would abort the whole host process */
  } else if (g_state == NOT_STARTED) {
    static char *argv_[] = { "hosi", "+RTS", "--install-signal-handlers=no", "-N2", "-RTS", NULL };
    char **argv = argv_;
    int argc = 5;
    hs_init_with_rtsopts(&argc, &argv);
    g_state = RUNNING;
  }
  gate_unlock();
  return rc;
}

void hosi_runtime_shutdown(void) {
  gate_lock();
  if (g_state != RUNNING) { gate_unlock(); return; }
  g_state = STOPPING;                       /* no new call gets in ... */
  while (g_inside > 0) gate_wait();         /* ... and those inside are waited for */
  gate_unlock();
  hosi_hs_close_all();                    /* every connection: what is waiting is answered CLOSED */
  hs_exit();
  gate_lock();
  g_state = ENDED;
  gate_unlock();
}

void hosi_thread_done(void) {
  int running;
  gate_lock();
  running = (g_state == RUNNING);
  gate_unlock();
  if (running) hs_thread_done();
}

uint32_t hosi_abi_version(void) { return HOSI_ABI_VERSION; }

/* ---- the connection ---------------------------------------------------------- */

/* A handle is a number the Haskell side gave out. One it does not know is refused there. */

hosi_conn *hosi_open(const char *uri, const hosi_opts *opts) {
  uint64_t h;
  (void)opts;
  if (!enter()) return NULL;
  h = hosi_hs_open(uri ? uri : "");
  leave();
  return (hosi_conn *)(uintptr_t)h;
}

void hosi_close(hosi_conn *c) {
  if (c == NULL || !enter()) return;
  hosi_hs_close((uint64_t)(uintptr_t)c);
  leave();
}

/* The Haskell side answers 0 (accepted), 1 (id in use), 2 (too many waiting), 3 (closed), 4 (not a handle). */
int hosi_submit(hosi_conn *c, hosi_id id,
           const uint8_t *req, size_t n, hosi_done cb, void *ud) {
  int r;
  if (c == NULL || cb == NULL || (req == NULL && n > 0)) return -EINVAL;
  if (!enter()) return -EPIPE;
  r = hosi_hs_submit((uint64_t)(uintptr_t)c, id, req, n, cb, ud);
  leave();
  switch (r) {
    case 0:  return 0;
    case 2:  return -EBUSY;
    case 3:  return -EPIPE;
    default: return -EINVAL;
  }
}

int hosi_cancel(hosi_conn *c, hosi_id id) {
  int r;
  if (c == NULL) return -EINVAL;
  if (!enter()) return -EPIPE;
  r = hosi_hs_cancel((uint64_t)(uintptr_t)c, id);
  leave();
  return r == 0 ? 0 : -EINVAL;
}
