module Recursion where

-- Z, S Z, S (S Z), S (S (S Z)) ...
data Numb = Z | S Numb deriving Show

isZero :: Numb -> Bool
isZero = \n -> case n of {Z -> True; S n' -> False}

isOne :: Numb -> Bool
-- equivalent to:
-- case n of {Z -> False; S n' -> case n' of {Z -> True; S n'' -> False}}
isOne = \n -> case n of {Z -> False; S n' -> isZero n'}

-- peel off one S and replace with two
double :: Numb -> Numb
double n = case n of {Z -> Z; S n' -> S (S (double n'))}

-- "peel off" S's from 2nd number until it becomes Z
-- S (add SSSSZ SZ)
-- S (S(add SSSSZ Z))
-- S (S(add SSSSZ))
add :: Numb -> Numb -> Numb
add m n = case n of {Z -> m; S n' -> S (add m n')}


data Shape = Roc | Pap | Sci deriving Show
data ShapeList = EmptySL | NonEmptySL Shape ShapeList deriving Show

listOf :: Shape -> Numb -> ShapeList
listOf sh n = 
  case n of 
  Z -> EmptySL
  S n' -> NonEmptySL sh (listOf sh n')

--addToEnd :: Shape -> ShapeList -> ShapeList
--addToEnd sh list
--  case list of
--  EmptySL -> NonEmptyList sh EmptySL
--  NonEmptySL sh list' -> NonEmptySL sh2 (addToEnd sh list')

mult :: Numb -> Numb -> Numb
mult m n = case n of 
  Z -> Z
  S n' -> add m (mult m n')

fac :: Numb -> Numb
fac n = case n of
  Z -> S Z
  S n' -> mult n (fac n')

sumsq :: Numb -> Numb
sumsq n = case n of
  Z -> Z
  S n' -> add (mult n n) (sumsq n')

power :: Numb -> Numb -> Numb
power m n = case n of 
  Z -> S Z
  S n' -> mult m (power m n')

data IntList = Empty | NonEmptyIL Int IntList deriving Show

multiply :: IntList -> Int
-- usage: multiply (NonEmptyIL 3 (NonEmptyIL 5 (NonEmptyIL ...)))
multiply n = case n of 
  EmptyIL -> 1
  NonEmptyIL n m -> n * (multiply m)