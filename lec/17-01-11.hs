module Basics where

f = (\x -> (\x -> 2 * x + x))

data Shape = Rock | Paper | Scissors deriving Show
data Result = Draw | Win Shape deriving Show

whatItBeats - \s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper}
