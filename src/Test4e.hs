module Test4e
where

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List

isAlpha ch = ('a' <= ch && ch <= 'z')

wrds :: T.Text -> [ T.Text ]
wrds bs = filter (not . T.null) $ T.split (not . isAlpha) bs

readDict = do
  allwords <- fmap (wrds . T.toLower) $ T.readFile "big.txt"
  let h = foldl' add H.empty allwords
      add h w = let c = H.lookupDefault (0 :: Int)  w h
                in  H.insert w (c+1) h
      member = \k -> H.member k h
      frequency = \k -> H.lookupDefault 0 k h
  return (member, frequency, T.pack)

