module Spl (nwords, correct)
    where

import Data.Char (toLower)
import Data.Ord (comparing)
import Data.List (maximumBy, splitAt, foldl')
import Text.Regex.TDFA (getAllTextMatches, (=~))

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

type NWords = Map.Map String Int


alphabet :: String
alphabet = enumFromTo 'a' 'z'

nwords :: String -> Map.Map String Int
nwords = train . words'

uniqueElems :: Ord a => [a] -> [a]
uniqueElems = uniq' Set.empty
    where uniq' _ [] = []
          uniq' seen (x:xs)
            | x `Set.member` seen = uniq' seen xs
            | otherwise           = x:uniq' (x `Set.insert` seen) xs

words' :: String -> [String]
words' = getAllTextMatches . flip (=~) "[a-z]+" . map toLower

train :: [String] -> NWords
train = foldl' populate Map.empty
    where populate m feature = Map.insertWith (+) feature 1 m

edits :: String -> [String]
edits word = uniqueElems $ concat [dels, trans, repl, ins]
    where dels   = [a ++ tail b | (a,b) <- splits, nn b]
          trans  = [ a ++ (b!!1):head b:tail (tail b) | (a,b) <- splits
                   , length b > 1]
          repl   = [a ++ c:tail b | (a,b) <- splits, c <- alphabet, nn b]
          ins    = [a ++ c:b | (a,b) <- splits, c <- alphabet]
          splits = [splitAt n word | n <- [0..length word]]
          nn     = not . null

knownEdits :: NWords -> String -> [String]
knownEdits nw word = uniqueElems [ e2 | e1 <- edits word, e2 <- edits e1
                                 , Map.member e2 nw]

known :: NWords -> [String] -> [String]
known nw = uniqueElems . filter (`Map.member` nw)

correct :: NWords -> String -> String
correct nw word = fst $ maximumBy (comparing snd) candidates
    where candidates = [(w, Map.findWithDefault 0 w nw) | w <- result]
          result     = head $ filter (not . null) start
          start      = [ known nw [word], known nw $ edits word
                       , knownEdits nw word , [word]]
