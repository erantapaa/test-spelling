module Test5
where

import qualified Data.HashMap.Strict as H
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
-- import qualified Data.Text as T
import Data.List
import Debug.Trace

isAlpha ch = ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')

wrds :: TL.Text -> [ TL.Text ]
wrds bs =
  let
      (_, r1) = TL.span (not . isAlpha) bs
      (w, r2) = TL.span isAlpha r1
  in if TL.null w then [] else TL.toLower w : wrds r2

readDict = do
  allwords <- fmap wrds $ TL.readFile "big.txt"
  let h = foldl' add H.empty allwords
      add h w = let c = H.lookupDefault (0 :: Int)  w h
                in  H.insert w (c+1) h
      member = \k -> H.member k h
      frequency = \k -> H.lookupDefault 0 k h
  return (member, frequency, TL.pack)

addints a b = trace "adding" (a+b)

