module FiniteState where

import Debug.Trace

data Numb = Z | S Numb deriving (Show,Eq)

type State = Int

data GrammarRule = Step State String State | End State deriving (Show,Eq)

data StrucDesc = NonLast State String StrucDesc | Last State deriving (Show,Eq)

-----------------------------------------------------------------
-- Sample grammars and structural descriptions

grammar1 :: [GrammarRule]
grammar1 = [Step 1 "the" 2,
            Step 2 "cat" 3,
            Step 2 "dog" 3,
            Step 1 "John" 3,
            Step 3 "chased" 4,
            Step 3 "admired" 4,
            Step 3 "left" 4,
            Step 3 "left" 6,
            Step 4 "the" 5,
            Step 5 "cat" 6,
            Step 5 "dog" 6,
            Step 4 "John" 6,
            End 6
            ]

sd1 :: StrucDesc
sd1 = NonLast 1 "the" (NonLast 2 "cat" (NonLast 3 "chased" (NonLast 4 "John" (Last 6))))

sd2 :: StrucDesc
sd2 = NonLast 1 "the" (NonLast 2 "cat" (NonLast 4 "John" (Last 6)))

-----------------------------------------------------------------
-- Some provided functions

-- successors grammar1 3 "left"    ~~>   [4,6]
-- successors grammar1 3 "admired" ~~>   [4]
-- successors grammar1 3 "cat"     ~~>   []
successors :: [GrammarRule] -> State -> String -> [State]
successors [] s w = []
successors (r:rs) s w =
    let resultFromRest = successors rs s w in
    case r of
    End x -> resultFromRest
    Step x w' y -> if s == x && w == w' then y : resultFromRest else resultFromRest

enders :: [GrammarRule] -> [State]
enders [] = []
enders (r:rs) =
    case r of
    End x -> x : (enders rs)
    Step x w y -> enders rs

firstState :: StrucDesc -> State
firstState (Last s) = s
firstState (NonLast s w sd) = s

wellFormed :: [GrammarRule] -> StrucDesc -> Bool
wellFormed g (Last s) = elem s (enders g)
wellFormed g (NonLast s w sd) = elem (firstState sd) (successors g s w) && wellFormed g sd

pf :: StrucDesc -> String
pf (Last s) = ""
pf (NonLast s w sd) =
    w ++ (
        case sd of
        Last s' -> ""
        NonLast s' w' sd' -> " "
    ) ++ pf sd

takeSteps :: [GrammarRule] -> State -> [String] -> [State]
takeSteps g s [] = [s]
takeSteps g s (w:ws) = concat (map (\s' -> takeSteps g s' ws) (successors g s w))

-----------------------------------------------------------------
-----------------------------------------------------------------
-- IMPORTANT: Do not change anything above here.

grammar2 :: [GrammarRule]
grammar2 = [Step 1 "these" 2,
            Step 1 "some" 2,
            Step 1 "they" 3,
            Step 1 "these" 3,
            Step 2 "dogs" 3,
            Step 2 "buffalo" 3,
            Step 3 "buffalo" 4,
            Step 3 "damaged" 4,
            Step 4 "damaged" 4,
            Step 4 "nice" 4,
            Step 4 "unicorns" 5,
            Step 4 "stuff" 5,
            Step 5 "and" 1,
            End 5
            ]

recognize :: [GrammarRule] -> State -> [String] -> Bool
recognize g s ws = any (\s -> elem s (enders g)) (takeSteps g s ws)

-- BEGIN functions for `generate`

predecessors :: [GrammarRule] -> State -> [(State, String)]
predecessors [] s = []
predecessors (r:rest) s =
    case r of
    End _ -> predecessors rest s
    Step p w n -> if n == s then ((p, w) : predecessors rest s) else predecessors rest s

extendByOne :: [GrammarRule] -> StrucDesc -> [StrucDesc]
extendByOne g sd = map (\(t,s) -> NonLast t s sd) (predecessors g (firstState sd))

extend :: [GrammarRule] -> Numb -> StrucDesc -> [StrucDesc]
extend g n sd = 
    case n of
    Z -> [sd]
    S n' -> sd : (concat (map (\sd' -> extend g n' sd') (extendByOne g sd)))

generate :: [GrammarRule] -> Numb -> [StrucDesc]
generate g n = 
    case n of 
    Z -> []
    S n' -> concat (map (\r -> extend g n' (Last r)) (enders g))
    
-- END functions for `generate`

parse :: [GrammarRule] -> State -> [String] -> [StrucDesc]
parse g s [] = if elem s (enders g) then [Last s] else []
parse g s (w:ws) = 
    map (\sd -> NonLast s w sd) (concat (map (\s' -> parse g s' ws) (successors g s w)))