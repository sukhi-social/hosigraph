{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hosigraph.Abi.Files
-- Description : The files that are written from the description (gen/), as text.
--
-- The generator program writes them or checks them ("Setu.Gen.runMain"), and the tests check
-- them too, from this one list.
module Hosigraph.Abi.Files (generatedFiles) where

import           Data.Text (Text)

import           Hosigraph.Abi.Api (api)
import           Setu.Gen.CAbi (cAbi, defaultCAbiConfig)
import           Setu.Gen.CHeader (cHeader)
import           Setu.Gen.HsExport (HsExportConfig(..), hsExport)

-- | Paths are relative to @gen/@.
generatedFiles :: [(FilePath, Text)]
generatedFiles =
  [ ("include/hosi.h",             cHeader api)
  , ("cbits/hosi_abi.c",           cAbi defaultCAbiConfig api)
  , ("Hosigraph/Abi/Generated.hs", hsExport hsCfg api)
  ]
  where
    hsCfg = HsExportConfig
      { hsModule        = "Hosigraph.Abi.Generated"
      , hsLibraryModule = "Hosigraph.Abi.Library"
      , hsLibraryValue  = "library"
      }
