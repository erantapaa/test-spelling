{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import qualified Spell as Spell
import qualified SpellText as SpellText
import qualified Test1 as T1
import qualified Test2 as T2
import qualified Test3 as T3
import qualified Test4 as T4
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

main :: IO ()
main = do
  (which : w : count : _) <- getArgs
  let n = read count
  let testbs fns = Spell.testRep fns n (BS.pack w)
      testtxt fns = SpellText.testRep fns n (T.pack w)

  case which of
    "1" -> T1.readDict >>= testbs
    "2" -> T2.readDict >>= testbs
    "3" -> T3.readDict >>= testbs
    "4" -> T4.readDict >>= testtxt
    _   -> error "unknown dictionary"

