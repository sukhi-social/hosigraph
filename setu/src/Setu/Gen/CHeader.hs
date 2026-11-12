-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Setu.Gen.CHeader
-- Description : The C header of the ticket ("番号札") ABI, written from an 'Api'.
--
-- The declarations follow section 6.1 of the requirements: a request is handed in with an id the
-- caller chooses, and the answer comes back once, through a callback.  Operations are named
-- inside the body, so the header does not mention them: adding an operation to the 'Api' leaves
-- the header as it was.  What the 'Api' changes is the prefix, the contract version and the name
-- of the one status it calls its own.
--
-- This module writes text.  No C is compiled here.
module Setu.Gen.CHeader
  ( cHeader
  , cShape, cIncludes, cDecls
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Setu.Spec

-- | The whole header file.
cHeader :: Api -> Text
cHeader api = T.intercalate "\n"
  [ T.unlines
      [ "/* " <> p <> ".h -- written by setu from the description of the " <> p <> " API. Do not edit. */"
      , "#ifndef " <> up <> "_H"
      , "#define " <> up <> "_H"
      ]
  , T.unlines
      [ "/* The contract version. It changes when this header, or what an operation takes or gives,"
      , "   changes in a way an older caller cannot follow. */"
      , "#define " <> up <> "_ABI_VERSION " <> T.pack (show (apiVersion api))
      ]
  , cIncludes
  , T.unlines ["#ifdef __cplusplus", "extern \"C\" {", "#endif"]
  , cDecls api
  , T.unlines
      [ "/* The contract version this library was built with (the same number as " <> up <> "_ABI_VERSION). */"
      , "uint32_t " <> p <> "_abi_version(void);"
      ]
  , T.unlines ["#ifdef __cplusplus", "}", "#endif"]
  , T.unlines ["#endif /* " <> up <> "_H */"]
  ]
  where p = apiName api; up = T.toUpper p

-- | What section 6.1 shows: the includes and the declarations, nothing around them.
cShape :: Api -> Text
cShape api = cIncludes <> "\n" <> cDecls api

cIncludes :: Text
cIncludes = T.unlines ["#include <stdint.h>", "#include <stddef.h>"]

-- | The types and functions of the ABI.
cDecls :: Api -> Text
cDecls api = T.intercalate "\n"
  [ T.unlines
      [ "typedef struct " <> p <> "_conn " <> p <> "_conn;"
      , "typedef struct " <> p <> "_opts " <> p <> "_opts;   /* the library's options; NULL means the defaults */"
      , "typedef uint64_t " <> p <> "_id;"
      ]
  , T.unlines $
      [ "/* How a call ended. The numbers are part of the ABI: OK is 0 and the others follow in this order. */"
      , "typedef enum {"
      , "  " <> up <> "_OK = 0,"
      ] ++ statusLines ++
      [ "} " <> p <> "_status;" ]
  , T.unlines
      [ "/* Called exactly once for each submit that returned 0, on a thread of the library, never on"
      , "   the thread that called submit, cancel or close. Keep it short: no blocking, no exception,"
      , "   no longjmp, and do not call close from it. buf is valid only during the call (NULL when n is 0). */"
      , "typedef void (*" <> p <> "_done)(" <> p <> "_id id, " <> p <> "_status st,"
      , indentTo ("typedef void (*" <> p <> "_done)(") <> "const uint8_t *buf, size_t n, void *ud);"
      ]
  , T.unlines
      [ "/* Once per process. 0 on success (also when it is already running). A start after a shutdown is"
      , "   refused (-EPERM). Starts the Haskell runtime without taking over the process's signals. */"
      , "int  " <> p <> "_runtime_init(void);"
      , "/* Only when the process is ending: closes every connection, then stops the runtime."
      , "   Calls that start after it are refused. */"
      , "void " <> p <> "_runtime_shutdown(void);"
      ]
  , T.unlines
      [ "/* Returns a handle at once; getting ready goes on in the background. uri is UTF-8 text and"
      , "   what it means is the library's to say. NULL opts means the defaults. A failure to get ready"
      , "   is reported to every request that waited for it. NULL if the runtime is not running. */"
      , pad openRet (p <> "_conn *") <> p <> "_open(const char *uri, const " <> p <> "_opts *opts);"
      , "/* Every request still waiting is answered with CLOSED. Returns after every callback has returned,"
      , "   so what ud points to may be freed after it. */"
      , pad openRet "void" <> p <> "_close(" <> p <> "_conn *c);"
      ]
  , T.unlines
      [ "/* Returns 0 if the request is accepted; then done is called exactly once."
      , "   Otherwise done is never called, and the result is a negative errno: -EINVAL (the id is already"
      , "   in use, or c is not an open connection), -EBUSY (too many requests are waiting), -EPIPE (the"
      , "   connection or the runtime is closed). The body is UTF-8 EDN and the ABI does not read it;"
      , "   it is copied before submit returns. Requests start in the order of submit and may finish"
      , "   in any order. */"
      , "int " <> p <> "_submit(" <> p <> "_conn *c, " <> p <> "_id id,"
      , indentTo ("int " <> p <> "_submit(") <> "const uint8_t *req, size_t n,"
      , indentTo ("int " <> p <> "_submit(") <> p <> "_done cb, void *ud);"
      ]
  , T.unlines
      [ "/* May be called any number of times (idempotent). If the cancel and the finish meet,"
      , "   exactly one of them is reported. Returns 0, or -EINVAL if c is not an open connection. */"
      , "int " <> p <> "_cancel(" <> p <> "_conn *c, " <> p <> "_id id);"
      ]
  , T.unlines
      [ "/* A thread that is done with the library may say so (optional). */"
      , "void " <> p <> "_thread_done(void);"
      ]
  ]
  where
    p  = apiName api
    up = T.toUpper p

    -- The one status the API names itself comes first after OK; the rest are fixed by the ABI.
    statuses =
      [ (apiAppErr api, apiAppErrDoc api)
      , ("PROTOCOL",  "a breach of the contract: a request that is not UTF-8, not EDN or not the shape the operation takes, or a broken wire protocol")
      , ("IO",        "reading or writing failed")
      , ("TIMEOUT",   "the request ran out of time")
      , ("CANCELLED", "cancelled by " <> p <> "_cancel")
      , ("CLOSED",    "the connection, or the runtime, was closed before the answer")
      , ("INTERNAL",  "something unexpected inside the library")
      ]
    nameW = maximum [T.length n | (n, _) <- statuses] + T.length (up <> "_ERR_") + 1   -- +1 for the comma
    statusLines =
      [ "  " <> pad nameW (up <> "_ERR_" <> n <> (if i == lastI then "" else ",")) <> " /* " <> d <> " */"
      | (i, (n, d)) <- zip [0 :: Int ..] statuses ]
    lastI = length statuses - 1
    openRet = T.length (p <> "_conn *")

pad :: Int -> Text -> Text
pad n = T.justifyLeft n ' '

-- | Spaces as wide as the text, to line up a wrapped parameter list.
indentTo :: Text -> Text
indentTo t = T.replicate (T.length t) " "
