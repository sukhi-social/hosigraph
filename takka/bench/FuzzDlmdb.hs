{-# LANGUAGE OverloadedStrings #-}
import qualified Takka.KV as KV  -- run: scripts/build-ghc.sh fuzz && build/dlmdb-fuzz
import qualified Data.ByteString as B
import qualified Data.Set as S
import Control.Monad
import System.Directory
import Test.QuickCheck

type KeyGen = Gen B.ByteString

alpha :: [Int] -> Int -> Int -> [Int] -> KeyGen
alpha letters lo hi pre = do
  n <- choose (lo, hi)
  body <- vectorOf n (elements letters)
  pure (B.pack (map fromIntegral (pre ++ body)))

-- one trial: insert distinct keys in a random order (one txn), optionally delete a random
-- subset in a second txn, then compare cursor scan / counts / ranks with a Data.Set model.
trial :: KV.DbiFlags -> Bool -> KeyGen -> Int -> IO Bool
trial flags counted g nKeys = do
  ks <- generate (vectorOf nKeys g)
  dels <- generate (sublistOf (S.toList (S.fromList ks)))
  let d = "/tmp/takka-fuzz"
  doesDirectoryExist d >>= \e -> when e (removeDirectoryRecursive d)
  KV.withEnv d KV.defaultEnvConfig $ \env -> do
    dbi <- KV.withWriteTxn env $ \t -> do
      x <- KV.openDbi t "t" flags
      forM_ ks $ \k -> KV.put t x k B.empty
      pure x
    unless (null dels) $ KV.withWriteTxn env $ \t -> forM_ dels $ \k -> KV.del t dbi k
    let model = S.toAscList (S.fromList ks `S.difference` S.fromList dels)
    KV.withReadTxn env $ \t -> do
      scanned <- map fst <$> KV.scanPrefix t dbi B.empty
      cOk <- if counted then (== fromIntegral (length model)) <$> KV.countAll t dbi else pure True
      rOk <- if counted
               then fmap and . forM (zip [0 ..] model) $ \(i, k) -> do
                      r1 <- KV.keyRank t dbi k
                      r2 <- KV.getRank t dbi i
                      pure (r1 == Just i && fmap fst r2 == Just k)
               else pure True
      gOk <- fmap and . forM model $ \k -> (/= Nothing) <$> KV.get t dbi k
      pure (scanned == model && cOk && rOk && gOk)

report :: String -> KV.DbiFlags -> Bool -> KeyGen -> Int -> Int -> IO ()
report label flags counted g nKeys trials = do
  rs <- replicateM trials (trial flags counted g nKeys)
  putStrLn (label ++ ": " ++ show (length (filter not rs)) ++ " / " ++ show trials ++ " trials corrupted")

main :: IO ()
main = do
  let pc  = KV.dbiPrefixCompression
      cnt = KV.dbiCounted
      both = KV.dbiCounted <> KV.dbiPrefixCompression
      generic = alpha [0,1,2,254,255] 1 4 []
      storeAve = alpha [0,1,2,254,255] 1 6 [0,0,0,2]            -- common 4-byte prefix, like our AVE
      storeEav = alpha [0,1,2,254,255] 1 6 [128,0,0,0,0,0,0,1]  -- like EAV
      oneByte = alpha [0,1,2,254,255] 1 5 [7]
      noZero  = alpha [1,2,3,254,255] 1 4 []
  putStrLn "--- 30 keys in random order, then random deletes; 200 trials each ---"
  report "prefix only        generic keys          " pc False generic 30 200
  report "prefix only        no zero bytes         " pc False noZero 30 200
  report "prefix only        common 1-byte prefix  " pc False oneByte 30 200
  report "prefix only        AVE-like (00 00 00 aid)" pc False storeAve 30 200
  report "prefix only        EAV-like (80 00 .. 01)" pc False storeEav 30 200
  report "counted+prefix     generic keys          " both True generic 30 200
  report "counted+prefix     AVE-like              " both True storeAve 30 200
  report "counted+prefix     EAV-like              " both True storeEav 30 200
  report "counted only       generic keys          " cnt True generic 30 200
  report "counted only       AVE-like              " cnt True storeAve 30 200
  report "counted only       EAV-like              " cnt True storeEav 30 200
  report "plain              generic keys          " KV.dbiPlain False generic 30 200
  putStrLn "--- bigger: 2000 keys per trial (multi-page), 60 trials ---"
  report "prefix only        generic keys          " pc False generic 2000 60
  report "prefix only        AVE-like              " pc False storeAve 2000 60
  report "counted+prefix     AVE-like              " both True storeAve 2000 60
  report "counted only       generic keys          " cnt True generic 2000 60
  report "counted only       AVE-like              " cnt True storeAve 2000 60
  report "counted only       EAV-like              " cnt True storeEav 2000 60
