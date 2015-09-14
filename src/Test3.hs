module Test3
where

import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Char8 as BS
import Data.List

isAlpha ch = 'a' <= ch && ch <= 'z'

wrds :: BS.ByteString -> [ BS.ByteString ]
wrds bs =
  let
      (_, r1) = BS.span (not . isAlpha) bs
      (w, r2) = BS.span isAlpha r1
  in if BS.null w then [] else w : wrds r2

readDict = do
  allwords <- fmap wrds $ BS.readFile "big.lower"
  let h = foldl' add H.empty allwords
      add h w = let c = H.lookupDefault (0 :: Int)  w h
                in  H.insert w (c+1) h
      member = \k -> H.member k h
      frequency = \k -> H.lookupDefault 0 k h
  return (member, frequency, BS.pack)

