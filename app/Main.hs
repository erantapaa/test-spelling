{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import qualified Spell.ByteString as SpellBS
import qualified Spell.Text as SpellT
import qualified Spell.LazyText as SpellLT
import qualified Test1 as T1
import qualified Test2 as T2
import qualified Test3 as T3
import qualified Test4 as T4
import qualified Test4b as T4b
import qualified Test5 as T5
import qualified Test6 as T6

main :: IO ()
main = do
  (which : w : count : _) <- getArgs
  let n = read count
      test_bs (m,f,p) = SpellBS.testRep (m,f) n (p w)
      test_txt (m,f,p) = SpellT.testRep (m,f) n (p w)
      test_ltxt (m,f,p) = SpellLT.testRep (m,f) n (p w)
  case which of 
    "1"  -> T1.readDict >>= test_bs
    "2"  -> T2.readDict >>= test_bs
    "3"  -> T3.readDict >>= test_bs
    "4"  -> T4.readDict >>= test_txt
    "4b" -> T4b.readDict >>= test_txt
    "5"  -> T5.readDict >>= test_ltxt
    "6"  -> T6.readDict >>= test_txt
    _    -> error "unknown implementation"

