module Test3a
where

import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as B
import Data.List

isAlpha ch = 'a' <= ch && ch <= 'z'

isAlpha' w = 97 <= w && w <= 122

wrds :: BS.ByteString -> [ BS.ByteString ]
-- wrds bs = filter (not . B.null) $ B.splitWith (not . isAlpha') bs
wrds bs = 
  let
      (_, r1) = BS.span (not . isAlpha) bs
      (w, r2) = BS.span isAlpha r1
  in if BS.null w then [] else w : wrds r2

toLower :: BS.ByteString -> BS.ByteString
toLower = B.map go
  where go ch | 65 <= ch && ch <= 90 = ch + 97 - 65
              | otherwise              = ch

readDict = do
  allwords <- fmap (wrds . toLower) $ BS.readFile "big.txt"
  let h = foldl' add H.empty allwords
      add h w = let c = H.lookupDefault (0 :: Int)  w h
                in  H.insert w (c+1) h
      member = \k -> H.member k h
      frequency = \k -> H.lookupDefault 0 k h
  return (member, frequency, BS.pack)

