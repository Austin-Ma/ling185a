module Assignment02 where

import Debug.Trace

data Numb = Z | S Numb deriving Show

add :: Numb -> Numb -> Numb
add m n = case n of {Z -> m; S n' -> S (add m n')}

one, two, three, four, five, six :: Numb
one = S Z
two = S one
three = S two
four = S three
five = S four
six = S five

data NumbList = EmptyNL | NonEmptyNL Numb NumbList deriving Show

list0, list1, list2 :: NumbList
list0 = NonEmptyNL one (NonEmptyNL two (NonEmptyNL three EmptyNL))
list1 = NonEmptyNL four (NonEmptyNL Z (NonEmptyNL two EmptyNL))
list2 = NonEmptyNL six (NonEmptyNL one (NonEmptyNL three (NonEmptyNL four EmptyNL)))

-----------------------------------------------------------------
-----------------------------------------------------------------
-- IMPORTANT: Do not change anything above here.
--            Write all your code below.
-----------------------------------------------------------------
-----------------------------------------------------------------

sumUpTo :: Numb -> Numb
-- computes the sum of all the numbers less than or equal to the given number.
sumUpTo x = case x of
  Z -> Z
  S x' -> (add x (sumUpTo x'))

equal :: Numb -> Numb -> Bool
-- returns True if the two numbers given are equal, and False otherwise.
equal x y = case x of
  Z -> case y of
       Z -> True
       S y' -> False
  S x' -> case y of 
          Z -> False
          S y' -> (equal x' y')

difference :: Numb -> Numb -> Numb
-- computes the absolute value of the difference between the two given numbers.
difference x y = case x of
  Z -> case y of
       Z -> Z
       S y' -> y
  S x' -> case y of 
          Z -> x
          S y' -> (difference x' y')

size :: NumbList -> Numb
-- computes the number of elements in a list.
size l = case l of
  EmptyNL -> Z
  NonEmptyNL l' l'' -> S (size l'')

lastElement :: NumbList -> Numb
-- computes the last element of the given list; or, if the list is empty, the 
-- result should be Z.
lastElement l = case l of
  EmptyNL -> Z
  NonEmptyNL l l' -> case l' of
                     EmptyNL -> l
                     _ -> lastElement l'

total :: NumbList -> Numb
-- computes the total sum of the elements of the given list.
total l = case l of 
  EmptyNL -> Z
  NonEmptyNL l l' -> add l (total l')

incrementAll :: Numb -> NumbList -> NumbList
-- adds the given number to each of the elements in the given list.
incrementAll n l = case l of 
  EmptyNL -> EmptyNL
  NonEmptyNL l l' -> NonEmptyNL (add n l) (incrementAll n l')

contains :: (Numb -> Bool) -> NumbList -> Bool
-- result of contains f list is True if there is at least one element of list 
-- on which f returns True, and is False otherwise.
contains f l = case l of
  EmptyNL -> False
  NonEmptyNL l l' -> if f l then True else contains f l'

remove :: (Numb -> Bool) -> NumbList -> NumbList
-- removes from the given list all the elements on which the given function 
-- returns True.
remove f l = case l of
  EmptyNL -> EmptyNL
  NonEmptyNL l l' -> if f l then remove f l' else NonEmptyNL l (remove f l')

append :: NumbList -> NumbList -> NumbList
-- produces a new list containing the elements of the two given lists combined
-- in the order given.
append l1 l2 = case l1 of
  EmptyNL -> l2
  NonEmptyNL l1 l1' -> NonEmptyNL l1 (append l1' l2)

prefix :: Numb -> NumbList -> NumbList
-- result of prefix n l is the length-n prefix of l; or, if n is greater than 
-- the length of l, the result should just be l itself.
prefix n l = case n of
  Z -> EmptyNL
  S n' -> case l of 
          EmptyNL -> EmptyNL
          NonEmptyNL l l' -> NonEmptyNL l (prefix n' l')
