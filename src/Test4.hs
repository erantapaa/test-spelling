module Test4
where

import qualified Data.HashMap.Strict as H
import qualified Spell as Spell
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List

isAlpha ch = ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')

wrds :: T.Text -> [ T.Text ]
wrds bs =
  let
      (_, r1) = T.span (not . isAlpha) bs
      (w, r2) = T.span isAlpha r1
  in if T.null w then [] else T.toLower w : wrds r2

readDict = do
  allwords <- fmap wrds $ T.readFile "big.txt"
  let h = foldl' add H.empty allwords
      add h w = let c = H.lookupDefault (0 :: Int)  w h
                in  H.insert w (c+1) h
      member = \k -> H.member k h
      frequency = \k -> H.lookupDefault 0 k h
  return (member, frequency)

