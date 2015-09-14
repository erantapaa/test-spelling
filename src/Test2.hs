module Test2
where

import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Char8 as BS
import Data.List

parseEntry bs = 
  let (w : vs : _) = BS.words bs
      v = maybe 0 fst (BS.readInt vs)
  in (w, v)

readDict = do
  entries <- fmap (map parseEntry . BS.lines) $ BS.readFile "words.count"
  let h = foldl' (\h (k,v) -> H.insert k v h) H.empty entries
      member = \k -> H.member k h
      frequency = \k -> H.lookupDefault 0 k h
  return (member, frequency, BS.pack)

