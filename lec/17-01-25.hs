module Bigrams where

-- data types for bigram grammars

data GrammarRule = Step String String | End String deriving (Show,Eq)
data StrucDesc = NonLast String StrucDesc | Last String deriving (Show,Eq)

grammar1 :: [GrammarRule]
grammar1 = [Step "the" "hamsters" -- can step from first to second el of bigram
            Step "the" "small"]

firstWord :: StrucDesc -> String
firstWord sd = 
  

wellFormed :: [GrammarRule] -> StrucDesc -> Bool
wellFormed g sd = 
  case sd of 
  Last s -> elem (End s) g
  NonLast s sd' -> elem (Stem s (firstWord sd')) g

extendByOne :: [GrammarRule] -> StrucDesc -> [StrucDesc]
extendByOne g sd = applyToAll func (predecessors g (firstWord sd))
-- func String -> StrucDesc


-- for r in g
-- if r is useful (useful if is (step __ (first word of sd)))
-- results = results + newSD(r, sol)

-- for w in wordsToUse
-- results = results + newSD(w, sd)

predecessors :: [GrammarRule] -> String -> [String]
predecessors g s = undefined

-- equivalent to the map function
applyToAll :: (a -> b) -> [a] -> [b] -- cf. map function
applyToAll f list -> 
  case list of 
  [] -> []
  (x:xs) (f x) : (applyToAll f xs)
