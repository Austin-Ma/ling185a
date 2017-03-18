module Util where

import qualified Data.Map as Map

------------------------------------------------------------

-- Feel free to ignore the implementation of this function
printMap :: (Show k, Show v) => Map.Map k v -> IO ()
printMap m = (putStr . unlines) (map show (Map.toList m))

------------------------------------------------------------

-- We've seen this one before
sumOver :: (a -> Double) -> [a] -> Double
sumOver f xs = sum (map f xs)

maxOver :: (a -> Double) -> [a] -> (Double,a)
maxOver f xs = 
  case xs of 
  [] -> undefined
  (x:xs) -> 
    -- check xs length to ensure we don't call fst with an undefined argument
    -- use gte comparison to return the first largest value in the list in case
    -- there are multiple equally largest values
    if length xs == 0 || f x >= fst (maxOver f xs) then 
      (f x, x) 
    else maxOver f xs

------------------------------------------------------------

allPairs :: [a] -> [b] -> [(a,b)]
allPairs l1 l2 = 
  case l1 of 
  [] -> []
  (h:t) -> map (\li -> (h,li)) l2 ++ allPairs t l2

------------------------------------------------------------

updateForAll :: (t -> c -> t) -> t -> [c] -> t
updateForAll f z xs = 
  case xs of 
  [] -> z
  (h:t) -> updateForAll f (f z h) t

