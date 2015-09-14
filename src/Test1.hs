{-# LANGUAGE OverloadedStrings #-}

module Test1
where

import qualified Dictionary as D
import qualified Data.ByteString.Char8 as BS

import Data.Monoid
import Data.List.Ordered (nubSort)
import Data.Ord
import Data.List
import Control.Monad

-- load dictionary and return access functions
readDict = do
  words <- BS.readFile "words.count"
  return (D.member words, D.lookupInt words, BS.pack)

