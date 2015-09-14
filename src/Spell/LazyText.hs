{-# LANGUAGE OverloadedStrings #-}

module Spell.LazyText
where

import qualified Data.Text.Lazy as TL
import Data.Text.Lazy (Text)
import Data.Monoid
import Data.List.Ordered (nubSort)
import Data.Ord
import Data.List
import Control.Monad

type Dict = ( TL.Text -> Bool, TL.Text -> Int )

singles :: [ TL.Text ]
singles = map TL.singleton ['a'..'z']

edits :: TL.Text -> [ TL.Text ]
edits w = deletes <> nubSort (transposes <> replaces) <> inserts
  where
    splits     = zip (TL.inits w) (TL.tails w)
    deletes    = [ a <> (TL.drop 1 b) | (a,b) <- splits, TL.length b > 0 ]
    transposes = [ a <> c <> (TL.drop 2 b) | (a,b) <- splits, TL.length b > 1,
                   let c = TL.pack [ TL.index b 1, TL.index b 0 ] ]
    replaces   = [ a <> c <> (TL.drop 1 b) | (a,b) <- splits, TL.length b > 1,
                    c <- singles ]
    inserts    = [ a <> c <> b | (a,b) <- splits, c <- singles ]

orElse :: [a] -> [a] -> [a]
orElse [] bs = bs
orElse as _  = as

-- | Correct a word. 'isMember' and 'frequency' are functions to
--   determine if a word is in the dictionary and to lookup its
--   frequency, respectively.
correct :: Dict -> TL.Text -> TL.Text
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
testRep :: Dict -> Int -> TL.Text -> IO ()
testRep dictfns n w0 = do
  replicateM_ n $ do
    print $ correct dictfns (foo n w0)

