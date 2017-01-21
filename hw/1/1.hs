data Shape = Rock | Paper | Scissors deriving Show
data Result = Draw | Win Shape deriving Show

let n = 1
let f = (\s -> case s of {Rock -> 334; Paper -> 138; Scissors -> 99})
let whatItBeats = \s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper}

-- A
((\x -> (\y -> y + (3 * x))) 4) 1
(\y -> y + (3 * 4)) 1       lambda reduction
(\y -> y + 12) 1            arithmetic
1 + 12                      lambda reduction
13                          arithmetic

-- B
((\x -> (\y -> x + (3 * x))) 4) 1
(\y -> 4 + (3 * 4)) 1       lambda reduction
(\y -> 4 + 12) 1            arithmetic
(\y -> 16) 1                arithmetic
16                          lambda reduction

-- C
((\x -> (\y -> y + (3 * y))) 4) 1
((\y -> y + (3 * y))) 1     lambda reduction
1 + (3 * 1)                 lambda reduction
1 + 3                       arithmetic
4                           arithmetic

-- D
(\y -> y + ((\y -> 3*y) 4)) 5
5 + ((\y -> 3*y) 4)         lambda reduction
5 + (3*4)                   lambda reduction
5 + 12                      arithmetic
17                          arithmetic

-- E
(\y -> ((\y -> 3*y) 4) + y) 5
((\y -> 3*y) 4) + 5         lambda reduction
(3*4) + 5                   lambda reduction
12 + 5                      arithmetic
17                          arithmetic

-- F
f ((\fn -> fn Rock) (\x -> whatItBeats x))
f ((\x -> whatItBeats x) Rock)  lambda reduction
f (whatItBeats Rock)            lambda reduction
f ((\s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper}) Rock)   substitution
f (case Rock of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper})             lambda reduction
f Scissors                                                                        case reduction
(\s -> case s of {Rock -> 334; Paper -> 138; Scissors -> 99}) Scissors            substitution
case Scissors of {Rock -> 334; Paper -> 138; Scissors -> 99}                      lambda reduction
99                              case reduction

-- G
whatItBeats (case Paper of {Rock -> Paper; Paper -> Rock; Scissors -> Scissors})
whatItBeats Rock                case reduction
(\s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper}) Rock       case reduction
case Rock of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper}                 lambda reduction
Scissors                        lambda reduction

-- H
(case (n+1) of {3 -> whatItBeats; 2 -> (\s -> Scissors)}) Paper
(case (1+1) of {3 -> whatItBeats; 2 -> (\s -> Scissors)}) Paper                   substitution
(case (1+1) of {3 -> whatItBeats; 2 -> (\s -> Scissors)}) Paper                   substitution
(case 2 of {3 -> whatItBeats; 2 -> (\s -> Scissors)}) Paper                       arithmetic
(\s -> Scissors) Paper          case reduction
Scissors                        lambda reduction

-- I
case (Win (whatItBeats Rock)) of {Draw -> n; Win x -> (n + f x)}
case (Win ((\s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper}) Rock)) of {Draw -> n; Win x -> (n + f x)}   substitution
case (Win ((\s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper}) Rock)) of {Draw -> n; Win x -> (n + f x)}   substitution
case (Win (case Rock of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper})) of {Draw -> n; Win x -> (n + f x)}             lambda substitution
case (Win Scissors of {Draw -> n; Win x -> (n + f x)})   case reduction
case (Win Scissors of {Draw -> 1; Win x -> (1 + f x)})   substitution
(Scissors -> (1 + f x))         case reduction
1 + f Scissors                  lambda reduction
1 + (\s -> case s of {Rock -> 334; Paper -> 138; Scissors -> 99}) Scissors        substitution
1 + (case Scissors of {Rock -> 334; Paper -> 138; Scissors -> 99})                lambda reduction
1 + 99                          case reduction
100                             arithmetic