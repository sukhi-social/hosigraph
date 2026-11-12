{-# LANGUAGE ScopedTypeVariables #-}
-- | A very small harness, shared by the test modules.
module Harness
  ( Test, check, checkEq, prop, patiently
  ) where

import           Control.Exception
import           Control.Monad
import           System.Timeout (timeout)
import           Test.QuickCheck

type Test = (String, IO ())

check :: String -> Bool -> IO ()
check msg ok = unless ok (throwIO (userError msg))

checkEq :: (Eq a, Show a) => String -> a -> a -> IO ()
checkEq msg expected actual =
  unless (expected == actual) $
    throwIO (userError (msg ++ "\n      expected: " ++ show expected ++ "\n      actual:   " ++ show actual))

prop :: Testable p => Int -> p -> IO ()
prop n p = do
  r <- quickCheckWithResult stdArgs { maxSuccess = n, chatty = False } p
  unless (isSuccess r) (throwIO (userError (output r)))

-- | A wait that must end: a test that would hang fails instead, and says what it waited for.
patiently :: String -> IO a -> IO a
patiently what act = do
  r <- timeout 20000000 act
  maybe (throwIO (userError ("gave up waiting for " ++ what))) pure r
