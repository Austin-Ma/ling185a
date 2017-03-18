module ProbCFG where

import qualified Util
import qualified Data.Map as Map
import qualified Data.List as List

import Debug.Trace

data Cat = S | NP | VP | N | D | V | PP | P | Adv deriving (Eq,Show,Ord)

data StrucDesc = Leaf Cat String | Binary Cat StrucDesc StrucDesc
                 deriving (Eq,Show)

type ProbCFG = ([(Cat,Double)],
                [((Cat,String),Double)],        -- terminal rules
                [((Cat,(Cat,Cat)),Double)],     -- nonterminal rules
                [Cat])

-- From Manning and Schutze, page 384
pcfg1 :: ProbCFG
pcfg1 = (   [(S,1.0)] ,
            [((P,"with"), 1.0), 
             ((V,"saw"), 1.0), 
             ((NP,"astronomers"), 0.1), ((NP,"ears"), 0.18), ((NP,"saw"), 0.04), ((NP,"stars"), 0.18), ((NP,"telescopes"), 0.1)] ,
            [((S,(NP,VP)), 1.0),
             ((PP,(P,NP)), 1.0),
             ((VP,(V,NP)), 0.7), ((VP,(VP,PP)), 0.3),
             ((NP,(NP,PP)), 0.4)] ,
            [S,NP,VP,PP,P,V]
        )

-- Like above but reversed probabilities on the rules for expanding VP
pcfg2 :: ProbCFG
pcfg2 = (   [(S,1.0)] ,
            [((P,"with"), 1.0), 
             ((V,"saw"), 1.0), 
             ((NP,"astronomers"), 0.1), ((NP,"ears"), 0.18), ((NP,"saw"), 0.04), ((NP,"stars"), 0.18), ((NP,"telescopes"), 0.1)] ,
            [((S,(NP,VP)), 1.0),
             ((PP,(P,NP)), 1.0),
             ((VP,(V,NP)), 0.3), ((VP,(VP,PP)), 0.7),
             ((NP,(NP,PP)), 0.4)] ,
            [S,NP,VP,PP,P,V]
        )

probLookup :: (Eq a) => [(a,Double)] -> a -> Double
probLookup []           key = 0.0
probLookup ((x,y):rest) key = if key == x then y else probLookup rest key

allCats :: ProbCFG -> [Cat]
allCats (starting,ending,transitions,cats) = cats

--------------------------------------------------
-- Utility functions for getting information from grammars.

startProb :: ProbCFG -> Cat -> Double
startProb (starting,ending,transitions,cats) = probLookup starting

endProb :: ProbCFG -> Cat -> String -> Double
endProb (starting,ending,transitions,cats) c s = probLookup ending (c,s)

trProb :: ProbCFG -> Cat -> (Cat,Cat) -> Double
trProb (starting,ending,transitions,cats) c (c1,c2) = probLookup transitions (c,(c1,c2))

-------------------------------------------------------------
-- Simple recursive definition of inside probabilities

leftDaughterCats :: ProbCFG -> Cat -> [Cat]
leftDaughterCats (starting,ending,transitions,cats) cat = 
    map (\((c,(l,r)),p) -> l) (filter (\((c,(l,r)),p) -> c == cat) transitions)

rightDaughterCats :: ProbCFG -> Cat -> [Cat]
rightDaughterCats (starting,ending,transitions,cats) cat = 
    map (\((c,(l,r)),p) -> r) (filter (\((c,(l,r)),p) -> c == cat) transitions)

naiveInside :: ProbCFG -> [String] -> Cat -> Double
naiveInside pcfg [] cat = undefined
naiveInside pcfg [w] cat = trace ("w,cat: " ++ show (w,cat)) $ endProb pcfg cat w
naiveInside pcfg w cat =
    trace ("w,cat: " ++ show (w,cat)) $
    Util.sumOver (\c1 -> Util.sumOver (\c2 -> Util.sumOver (\i -> trProb pcfg cat (c1,c2) * naiveInside pcfg (take i w) c1 * naiveInside pcfg (drop i w) c2) [1..(length w-1)]) (rightDaughterCats pcfg cat)) (leftDaughterCats pcfg cat)

allTriples :: [a] -> [b] -> [c] -> [(a,b,c)]
allTriples xs ys zs = map (\((x,y),z) -> (x,y,z)) (Util.allPairs (Util.allPairs xs ys) zs)

-------------------------------------------------------------

prefixes :: [a] -> [[a]]
prefixes [] = [[]]
prefixes x = map (\i -> take i x) [0..(length x)]

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes (x:xs) = (x:xs) : (suffixes xs)

-- returns all contiguous sublists of a sentence
sublists :: (Eq a) => [a] -> [[a]]
sublists [] = [[]]
sublists l = List.nub (concatMap (\li -> suffixes li) (prefixes l))

-- sorting function that orders by list size ascending
shortestFirst :: [a] -> [a] -> Ordering
shortestFirst a b =
    if length a > length b then GT
    else
        if length b > length a then LT
        else EQ

-- Produces a list of sentence-chunks, in the smallest-to-largest 
-- order in which their corresponding cells should be filled.
chunks :: [String] -> [[String]]
chunks sent = List.sortBy shortestFirst (filter (\l -> length l > 0) (sublists sent))

-- Produces a list of cells in the order in which they 
-- should be filled.
cellsToFill :: ProbCFG -> [String] -> [([String],Cat)]
cellsToFill pcfg sent = Util.allPairs (chunks sent) (allCats pcfg)

-------------------------------------------------------------

-- A name for the specific Map type that we will use to represent 
-- a table of inside probabilities.
type InsideTable = Map.Map ([String],Cat) Double

buildTableInside :: ProbCFG -> [String] -> InsideTable
buildTableInside pcfg sent =
    Util.updateForAll (fillCellInside pcfg) Map.empty (cellsToFill pcfg sent)

fillCellInside :: ProbCFG -> InsideTable -> ([String],Cat) -> InsideTable
fillCellInside pcfg tbl (chunk,cat) =
    --trace ("chunk,cat: " ++ show (chunk,cat)) $
    case chunk of
    [] -> undefined
    [w] ->
        let result = endProb pcfg cat w in
        if result > 0 then
            Map.insert (chunk,cat) result tbl
        else
            tbl
    w ->
        let insideProb = \ys -> \cat -> Map.findWithDefault 0 (ys,cat) tbl in
        let result = Util.sumOver (\c1 -> Util.sumOver (\c2 -> Util.sumOver (\i -> trProb pcfg cat (c1,c2) * insideProb (take i w) c1 * insideProb (drop i w) c2) [1..(length w-1)]) (rightDaughterCats pcfg cat)) (leftDaughterCats pcfg cat) in
        if result > 0 then
            Map.insert (chunk,cat) result tbl
        else
            tbl

-------------------------------------------------------------

-- A name for the specific Map type that we will use to represent 
-- a table of inside probabilities.
type ViterbiTable = (Map.Map ([String],Cat) Double, Map.Map ([String],Cat) (Cat,Cat,Int))

buildTableViterbi :: ProbCFG -> [String] -> ViterbiTable
buildTableViterbi pcfg sent = 
    Util.updateForAll (fillCellViterbi pcfg) (Map.empty, Map.empty) (cellsToFill pcfg sent)

fillCellViterbi :: ProbCFG -> ViterbiTable -> ([String],Cat) -> ViterbiTable
fillCellViterbi = undefined

-------------------------------------------------------------

-- Construct the best tree whose root is the given category and whose leaves 
-- produce the given word-sequence, based on a provided table of viterbi backpointers.
extractTree :: ([String],Cat) -> ViterbiTable -> StrucDesc
extractTree = undefined

