module ProbFSA where

type State = Int

type ProbFSA = ([(State,Double)],                   -- start distribution
                [(State,Double)],                   -- ending probabilities
                [((State,State),Double)],           -- transition probabilities
                [(((State,State),String),Double)],  -- emissions
                [State])                            -- all states

-- This is a probabilistic version of a grammar from Assignment #4.
pfsa1 :: ProbFSA
pfsa1 = (   -- start distribution
            [(1,1.0)] ,
            -- end probabilities
            [(5,0.5)] ,
            -- transition probabilities
            [((1,2), 0.3), ((1,3), 0.7),
             ((2,3), 1.0),
             ((3,4), 1.0),
             ((4,4), 0.4),
             ((4,5), 0.6),
             ((5,1), 0.5)] ,
            -- emission probabilities
            [(((1,2),"these"), 0.5), (((1,2),"some"), 0.5),
             (((1,3),"they"), 0.4), (((1,3),"these"), 0.6),
             (((2,3),"dogs"), 0.3), (((2,3),"buffalo"), 0.7),
             (((3,4),"buffalo"), 0.6), (((3,4),"damaged"), 0.4),
             (((4,4),"damaged"), 0.7), (((4,4),"nice"), 0.3),
             (((4,5),"unicorns"), 0.8), (((4,5),"stuff"), 0.2),
             (((5,1),"and"),0.1)],
            [1,2,3,4,5]
        )

-- This is the grammar we worked with in class this week.
pfsa2 :: ProbFSA
pfsa2 = (   -- start distribution
            [(100,1.0)] ,
            -- end probabilities
            [(400,0.5)] ,
            -- transition probabilities
            [((100,200), 0.4), ((100,300), 0.6),
             ((200,400), 1.0),
             ((300,200), 0.3), ((300,400), 0.7),
             ((400,300), 0.5)] ,
            -- emission probabilities
            [(((100,200),"c"), 1.0), 
             (((100,300),"a"), 0.7), (((100,300),"b"), 0.3), 
             (((200,400),"a"), 0.2), (((200,400),"d"), 0.8), 
             (((300,200),"c"), 0.6), (((300,200),"d"), 0.4), 
             (((300,400),"b"), 0.5), (((300,400),"c"), 0.5), 
             (((400,300),"b"), 0.3), (((400,300),"d"), 0.7)] ,
            [100,200,300,400]
        )

-- This is the grammar introduced at the beginning of Assignment #9.
pfsa3 :: ProbFSA
pfsa3 = (   -- start distribution
            [(10,1.0)] ,
            -- end probabilities
            [(40,0.5)] ,
            -- transition probabilities
            [((10,20), 0.4), ((10,30), 0.6),
             ((20,40), 1.0),
             ((30,40), 1.0),
             ((40,10), 0.5)] ,
            -- emission probabilities
            [(((10,20),"a"), 0.2), (((10,20),"b"), 0.8), 
             (((10,30),"a"), 0.7), (((10,30),"c"), 0.3), 
             (((20,40),"c"), 0.6), (((20,40),"d"), 0.4), 
             (((30,40),"b"), 0.1), (((30,40),"d"), 0.9), 
             (((40,10),"e"), 1.0)] ,
            [10,20,30,40]
        )

probLookup :: (Eq a) => [(a,Double)] -> a -> Double
probLookup []           key = 0.0
probLookup ((x,y):rest) key = if key == x then y else probLookup rest key

allStates :: ProbFSA -> [State]
allStates (starting,ending,transitions,emissions,states) = states

--------------------------------------------------
-- Utility functions for getting information from grammars.

startProb :: ProbFSA -> State -> Double
startProb (starting,ending,transitions,emissions,states) = probLookup starting

endProb :: ProbFSA -> State -> Double
endProb (starting,ending,transitions,emissions,states) = probLookup ending

trProb :: ProbFSA -> State -> State -> Double
trProb (starting,ending,transitions,emissions,states) st1 st2 = probLookup transitions (st1,st2)

emProb :: ProbFSA -> (State,State) -> String -> Double
emProb (starting,ending,transitions,emissions,states) (st1,st2) str = probLookup emissions ((st1,st2),str)

startOK :: ProbFSA -> State -> Bool
startOK pfsa st = startProb pfsa st > 0

endOK :: ProbFSA -> State -> Bool
endOK pfsa st = endProb pfsa st > 0

trOK :: ProbFSA -> State -> State -> Bool
trOK pfsa s1 s2 = trProb pfsa s1 s2 > 0

emOK :: ProbFSA -> (State,State) -> String -> Bool
emOK pfsa (s1,s2) str = emProb pfsa (s1,s2) str > 0

--------------------------------------------------

-- any :: (a -> Bool) -> [a] -> Bool
-- any f xs = elem True (map f xs)

sumOver :: (a -> Double) -> [a] -> Double
sumOver f xs = sum (map f xs)

--------------------------------------------------
-- Forwards and backwards functions, from class on Wed 3/8.

recognizeBackward :: ProbFSA -> [String] -> State -> Bool
recognizeBackward pfsa []     st = endOK pfsa st
recognizeBackward pfsa (w:ws) st =
    any (\next -> trOK pfsa st next && emOK pfsa (st,next) w && recognizeBackward pfsa ws next) (allStates pfsa)

probBackward :: ProbFSA -> [String] -> State -> Double
probBackward pfsa []     st = endProb pfsa st
probBackward pfsa (w:ws) st =
    sumOver (\next -> trProb pfsa st next * emProb pfsa (st,next) w * probBackward pfsa ws next) (allStates pfsa)

recognizeForward :: ProbFSA -> [String] -> State -> Bool
recognizeForward pfsa output st =
    if output == [] then
        startOK pfsa st
    else
        let (ws,w) = (init output, last output) in
        any (\prev -> recognizeForward pfsa ws prev && trOK pfsa prev st && emOK pfsa (prev,st) w) (allStates pfsa)

probForward :: ProbFSA -> [String] -> State -> Double
probForward pfsa output st =
    if output == [] then
        startProb pfsa st
    else
        let (ws,w) = (init output, last output) in
        sumOver (\prev -> probForward pfsa ws prev * trProb pfsa prev st * emProb pfsa (prev,st) w) (allStates pfsa)

--------------------------------------------------
-- IMPORTANT: Please do not change anything above here.

maxOver :: (a -> Double) -> [a] -> Double
maxOver f xs = maximum (map f xs)

viterbiProb :: ProbFSA -> [String] -> State -> Double
viterbiProb pfsa output st =
    if output == [] then
        startProb pfsa st
    else
        let (ws,w) = (init output, last output) in
        maxOver (\prev -> viterbiProb pfsa ws prev * trProb pfsa prev st * emProb pfsa (prev,st) w) (allStates pfsa)

-- helper function that finds the maximum Double in a list of (Double,State)
-- tuples and returns the full tuple
maxOverTuple :: [(Double,State)] -> (Double,State)
maxOverTuple l = case l of
    [] -> (0, undefined )
    (l:ls) -> if fst l > fst (maxOverTuple ls) then l else maxOverTuple ls

viterbiPair :: ProbFSA -> [String] -> State -> (Double,State)
viterbiPair pfsa output st =
    if output == [] then
        (startProb pfsa st, undefined)
    else
        let (ws,w) = (init output, last output) in
        maxOverTuple (map (\prev -> (((fst (viterbiPair pfsa ws prev)) * trProb pfsa prev st * emProb pfsa (prev,st) w), prev)) (allStates pfsa))

-- highest probability sequence: 1 -> 3 -> 4 -> 4 -> 5
-- I figured it out by calling veterbiPair starting with the final state (5)
-- and full sentence (["these","buffalo","damaged","stuff"]) and feeding the 
-- function's outputted state (second item in the tuple) back into the
-- function, incrementally removing one word from the end of the list each 
-- time. Example below:
-- *ProbFSA> viterbiPair pfsa1 ["these","buffalo","damaged","stuff"] 5
-- (8.4672e-3,4)
-- *ProbFSA> viterbiPair pfsa1 ["these","buffalo","damaged"] 4
-- (7.056e-2,4)
-- *ProbFSA> viterbiPair pfsa1 ["these","buffalo"] 4
-- (0.252,3)
-- *ProbFSA> viterbiPair pfsa1 ["these"] 3
-- (0.42,1)