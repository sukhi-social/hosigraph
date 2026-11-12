-- | Writes the files that are made from the description in "Hosigraph.Abi.Api":
--
-- > cabal run hosigraph-abi-gen -- --out   hosigraph-abi/gen    # write them
-- > cabal run hosigraph-abi-gen -- --check hosigraph-abi/gen    # fail if they are out of date
module Main (main) where

import           Hosigraph.Abi.Files (generatedFiles)
import           Setu.Gen (runMain)

main :: IO ()
main = runMain generatedFiles
