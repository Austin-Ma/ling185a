module FiniteState where

data Numb = Z | S Numb deriving Show

type State = Int

data GrammarRule = Step String String | End String deriving (Show,Eq)

takeSteps :: [GrammarRule] -> State -> [String] -> [State]
takeSteps g s [] = [s]
takeSteps g s (w:ws) = concat (map (\s' -> takeSteps g s' ws) (successors g s w))