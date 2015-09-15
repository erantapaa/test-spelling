module Test6
where

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List
import Debug.Trace
import System.IO

isAlpha ch = ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')

wrds :: T.Text -> [ T.Text ]
wrds bs =
  let
      (_, r1) = T.span (not . isAlpha) bs
      (w, r2) = T.span isAlpha r1
  in if T.null w then [] else T.toLower w : wrds r2

readDict = do
  let add h w = let c = H.lookupDefault (0 :: Int)  w h
                in  H.insert w (c+1) h
      loop h fh = do b <- hIsEOF fh
                     if b then return h
                          else do cs <- T.hGetLine fh
                                  let h' = foldl' add h (wrds cs)
                                  loop h' fh
  h <- withFile "big.txt" ReadMode (loop H.empty)
  let member = \k -> H.member k h
      frequency = \k -> H.lookupDefault 0 k h
  return (member, frequency, T.pack)

