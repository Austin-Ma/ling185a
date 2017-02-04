module LoopDetection where

import Bigrams

type Position = ([String], String)

canLoop :: [GrammarRule] -> String -> Bool

canLoop' :: [GrammarRule] -> Position -> Bool
canLoop' g (history, w) = 
  -- everything we might move to next after w
  let nextWords = successors g w in
  if (\nextWord -> elem nextWord history) nextWords then True
  -- otherwise, create Position representing having moved to each nextWord
  -- and see if we can reach a loop from any of those
  -- the call to 'any' will return False whenever nextWords is empty
  else 
    let newPositions = map (\nextWord -> (history ++ [w], nextWord)) nextWords in
    any (canLoop' g) newPositions