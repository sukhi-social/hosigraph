-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Setu.Gen.CAbi
-- Description : The C layer in front of the Haskell exports, written from an 'Api'.
--
-- Two things need C, and this is where they are.
--
-- 1. /Starting the Haskell runtime./  Haskell cannot call @hs_init@ itself (cabal's own
--    documentation says so: the author writes the function that does).  The runtime is started
--    with @--install-signal-handlers=no@, always: without it the RTS takes SIGINT from the host
--    (Go, the BEAM, Python) and kills the call that is running with @interrupted@.
-- 2. /Guarding the doors./  The runtime cannot be started again after @hs_exit@, and a call into
--    Haskell before @hs_init@ or after @hs_exit@ takes the whole process down (both seen by
--    running it, in the S0 spike).  So every public function lets a call through only while the
--    runtime runs, and @shutdown@ refuses new calls and waits for those in progress.
--
-- The C that is written is plain C99 with pthreads (Linux and macOS).  Nothing in it knows what
-- the API does.
module Setu.Gen.CAbi
  ( CAbiConfig(..), defaultCAbiConfig
  , cAbi
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Setu.Spec

newtype CAbiConfig = CAbiConfig
  { rtsFlags :: [Text]   -- ^ flags for the Haskell runtime, besides the signal one that is always given
  } deriving (Eq, Show)

defaultCAbiConfig :: CAbiConfig
defaultCAbiConfig = CAbiConfig { rtsFlags = ["-N2"] }

cAbi :: CAbiConfig -> Api -> Text
cAbi cfg api = T.unlines
  [ "/* " <> p <> "_abi.c -- written by setu from the description of the " <> p <> " API. Do not edit."
  , " *"
  , " * The functions of " <> p <> ".h, around the Haskell ones (" <> p <> "_hs_*, in the generated Haskell module)."
  , " * Two jobs: start the Haskell runtime, and let a call through only while it runs. */"
  , "#include <HsFFI.h>"
  , "#include <Rts.h>      /* hs_init_with_rtsopts is declared here, not in HsFFI.h */"
  , "#include <errno.h>"
  , "#include <pthread.h>"
  , "#include <stddef.h>"
  , "#include <stdint.h>"
  , "#include \"" <> p <> ".h\""
  , ""
  , "/* The Haskell half. The types are those of the Haskell signatures. */"
  , "extern uint64_t " <> p <> "_hs_open(const char *uri);"
  , "extern void     " <> p <> "_hs_close(uint64_t h);"
  , "extern int      " <> p <> "_hs_submit(uint64_t h, uint64_t id, const uint8_t *req, size_t n, " <> p <> "_done cb, void *ud);"
  , "extern int      " <> p <> "_hs_cancel(uint64_t h, uint64_t id);"
  , "extern void     " <> p <> "_hs_close_all(void);"
  , ""
  , "/* ---- the gate ---------------------------------------------------------------- */"
  , ""
  , "enum { NOT_STARTED, RUNNING, STOPPING, ENDED };"
  , ""
  , "static int             g_state  = NOT_STARTED;"
  , "static unsigned        g_inside = 0;          /* calls into Haskell in progress */"
  , "static pthread_mutex_t g_mu     = PTHREAD_MUTEX_INITIALIZER;"
  , "static pthread_cond_t  g_idle   = PTHREAD_COND_INITIALIZER;"
  , ""
  , "static int enter(void) {"
  , "  int ok;"
  , "  pthread_mutex_lock(&g_mu);"
  , "  ok = (g_state == RUNNING);"
  , "  if (ok) g_inside++;"
  , "  pthread_mutex_unlock(&g_mu);"
  , "  return ok;"
  , "}"
  , ""
  , "static void leave(void) {"
  , "  pthread_mutex_lock(&g_mu);"
  , "  if (--g_inside == 0) pthread_cond_broadcast(&g_idle);"
  , "  pthread_mutex_unlock(&g_mu);"
  , "}"
  , ""
  , "/* ---- the runtime ------------------------------------------------------------- */"
  , ""
  , "int " <> p <> "_runtime_init(void) {"
  , "  int rc = 0;"
  , "  pthread_mutex_lock(&g_mu);"
  , "  if (g_state == STOPPING || g_state == ENDED) {"
  , "    rc = -EPERM;            /* not hs_init: the RTS would abort the whole host process */"
  , "  } else if (g_state == NOT_STARTED) {"
  , "    static char *argv_[] = { \"" <> p <> "\", \"+RTS\", \"--install-signal-handlers=no\", " <> flags <> "\"-RTS\", NULL };"
  , "    char **argv = argv_;"
  , "    int argc = " <> T.pack (show (length (rtsFlags cfg) + 4)) <> ";"
  , "    hs_init_with_rtsopts(&argc, &argv);"
  , "    g_state = RUNNING;"
  , "  }"
  , "  pthread_mutex_unlock(&g_mu);"
  , "  return rc;"
  , "}"
  , ""
  , "void " <> p <> "_runtime_shutdown(void) {"
  , "  pthread_mutex_lock(&g_mu);"
  , "  if (g_state != RUNNING) { pthread_mutex_unlock(&g_mu); return; }"
  , "  g_state = STOPPING;                       /* no new call gets in ... */"
  , "  while (g_inside > 0) pthread_cond_wait(&g_idle, &g_mu);   /* ... and those inside are waited for */"
  , "  pthread_mutex_unlock(&g_mu);"
  , "  " <> p <> "_hs_close_all();                    /* every connection: what is waiting is answered CLOSED */"
  , "  hs_exit();"
  , "  pthread_mutex_lock(&g_mu);"
  , "  g_state = ENDED;"
  , "  pthread_mutex_unlock(&g_mu);"
  , "}"
  , ""
  , "void " <> p <> "_thread_done(void) {"
  , "  int running;"
  , "  pthread_mutex_lock(&g_mu);"
  , "  running = (g_state == RUNNING);"
  , "  pthread_mutex_unlock(&g_mu);"
  , "  if (running) hs_thread_done();"
  , "}"
  , ""
  , "uint32_t " <> p <> "_abi_version(void) { return " <> up <> "_ABI_VERSION; }"
  , ""
  , "/* ---- the connection ---------------------------------------------------------- */"
  , ""
  , "/* A handle is a number the Haskell side gave out. One it does not know is refused there. */"
  , ""
  , p <> "_conn *" <> p <> "_open(const char *uri, const " <> p <> "_opts *opts) {"
  , "  uint64_t h;"
  , "  (void)opts;"
  , "  if (!enter()) return NULL;"
  , "  h = " <> p <> "_hs_open(uri ? uri : \"\");"
  , "  leave();"
  , "  return (" <> p <> "_conn *)(uintptr_t)h;"
  , "}"
  , ""
  , "void " <> p <> "_close(" <> p <> "_conn *c) {"
  , "  if (c == NULL || !enter()) return;"
  , "  " <> p <> "_hs_close((uint64_t)(uintptr_t)c);"
  , "  leave();"
  , "}"
  , ""
  , "/* The Haskell side answers 0 (accepted), 1 (id in use), 2 (too many waiting), 3 (closed), 4 (not a handle). */"
  , "int " <> p <> "_submit(" <> p <> "_conn *c, " <> p <> "_id id,"
  , "           const uint8_t *req, size_t n, " <> p <> "_done cb, void *ud) {"
  , "  int r;"
  , "  if (c == NULL || cb == NULL || (req == NULL && n > 0)) return -EINVAL;"
  , "  if (!enter()) return -EPIPE;"
  , "  r = " <> p <> "_hs_submit((uint64_t)(uintptr_t)c, id, req, n, cb, ud);"
  , "  leave();"
  , "  switch (r) {"
  , "    case 0:  return 0;"
  , "    case 2:  return -EBUSY;"
  , "    case 3:  return -EPIPE;"
  , "    default: return -EINVAL;"
  , "  }"
  , "}"
  , ""
  , "int " <> p <> "_cancel(" <> p <> "_conn *c, " <> p <> "_id id) {"
  , "  int r;"
  , "  if (c == NULL) return -EINVAL;"
  , "  if (!enter()) return -EPIPE;"
  , "  r = " <> p <> "_hs_cancel((uint64_t)(uintptr_t)c, id);"
  , "  leave();"
  , "  return r == 0 ? 0 : -EINVAL;"
  , "}"
  ]
  where
    p     = apiName api
    up    = T.toUpper p
    flags = T.concat [ "\"" <> f <> "\", " | f <- rtsFlags cfg ]
