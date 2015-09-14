{-# LANGUAGE OverloadedStrings #-}

module SpellText
where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid
import Data.List.Ordered (nubSort)
import Data.Ord
import Data.List
import Control.Monad

type Dict = ( Text -> Bool, Text -> Int )

singles :: [ Text ]
singles = map T.singleton ['a'..'z']

edits :: Text -> [ Text ]
edits w = deletes <> nubSort (transposes <> replaces) <> inserts
  where
    splits     = zip (T.inits w) (T.tails w)
    deletes    = [ a <> (T.drop 1 b) | (a,b) <- splits, T.length b > 0 ]
    transposes = [ a <> c <> (T.drop 2 b) | (a,b) <- splits, T.length b > 1,
                   let c = T.pack [ T.index b 1, T.index b 0 ] ]
    replaces   = [ a <> c <> (T.drop 1 b) | (a,b) <- splits, T.length b > 1,
                    c <- singles ]
    inserts    = [ a <> c <> b | (a,b) <- splits, c <- singles ]

orElse :: [a] -> [a] -> [a]
orElse [] bs = bs
orElse as _  = as

-- | Correct a word. 'isMember' and 'frequency' are functions to
--   determine if a word is in the dictionary and to lookup its
--   frequency, respectively.
correct :: Dict -> Text -> Text 
correct (isMember,frequency) w0 = 
  let ed0 = [ w0 ]
      ed1 = edits w0
      ed2 = [ e2 | e1 <- ed1, e2 <- edits e1 ]

      kn0 = filter isMember ed0
      kn1 = filter isMember ed1
      kn2 = filter isMember ed2

      candidates = kn0 `orElse` (kn1 `orElse` (kn2 `orElse` [w0]))
  in maximumBy (comparing frequency) candidates

-- helper function to ensure that ghc doesn't optimize calls
-- to 'correct'
foo n w0 | n > 0 = w0 <> ""
         | otherwise = w0

-- test correcting a word multiple times (for timing)
testRep dictfns n w0 = do
  replicateM_ n $ do
    print $ correct dictfns (foo n w0)

