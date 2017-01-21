module Recursion where

data Numb = Z | S Numb deriving Show

mult :: Numb -> Numb -> Numb
