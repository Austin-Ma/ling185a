module ProbBigrams where

import qualified Corpus01
import qualified Corpus02a
import qualified Corpus02b
import qualified Data.Map as Map

tinySample = ["the dog chased the cat", "the cat bit the rat"]

-----------------------------------------------------------------
-- Utility functions that you can safely ignore the details of.

showMap :: (Show a) => Map.Map a Int -> [String]
showMap m = Map.foldMapWithKey (\k -> \v -> [show k ++ " --> " ++ show v]) m

addOne :: (Ord a) => a -> Map.Map a Int -> Map.Map a Int
addOne x m =
    if Map.member x m then
        Map.adjust (\n -> n+1) x m
    else
        Map.insert x 1 m

perplexity :: Double -> [String] -> Double
perplexity p test =
    let n = sum (map (\s -> length s + 1) test) in
    p ** (- 1.0 / fromIntegral n)

-----------------------------------------------------------------
-- Common functions that you should make sure you understand.

-- Converts a space-separated sentence into a list of words, plus 
-- the start and end markers.
prep :: String -> [String]
prep sent = ["<s>"] ++ words sent ++ ["</s>"]

-- trainModel produces a trained model on the basis of some training data.
-- The first argument (addBigram) is a function that can update such a model on the basis of 
-- some particular bigram observed in the training corpus.
-- The second argument is the training corpus (a list of sentences).
-- The third argument is an existing model that we wish to ``update'' with training data.
trainModel :: ((String,String) -> a -> a) -> [String] -> a -> a
trainModel addBigram [] m = m
trainModel addBigram (s:ss) m = updateForSent addBigram (prep s) (trainModel addBigram ss m)

-- updateForSent is like trainModel, but for a single sentence represented as a 
-- list of words (with start and end markers already present).
updateForSent :: ((String,String) -> a -> a) -> [String] -> a -> a
updateForSent addBigram [] m = m
updateForSent addBigram (w:[]) m = m
updateForSent addBigram (w:x:rest) m = addBigram (w,x) (updateForSent addBigram (x:rest) m)

-- probOfSent calculates the probability of a full sentence.
-- The first argument is a function that returns the probability of a given bigram.
-- The second argument is a sentence represented as a list of words (with start and 
-- end markers already present).
probOfSent :: ((String,String) -> Double) -> [String] -> Double
probOfSent f [] = 1.0
probOfSent f (w:[]) = 1.0
probOfSent f (w:x:rest) = f (w,x) * probOfSent f (x:rest)

-- probOfSents is like probOfSents, but for a list of sentences.
probOfSents :: ((String,String) -> Double) -> [String] -> Double
probOfSents f sents = product (map (\sent -> probOfSent f (prep sent)) sents)

-----------------------------------------------------------------
-- The ``simple'' bigram model

type SimpleModel = (Map.Map String Int, Map.Map (String,String) Int)

freshSimpleModel :: SimpleModel
freshSimpleModel = (Map.empty, Map.empty)

addBigramSimple :: (String,String) -> SimpleModel -> SimpleModel
addBigramSimple (x,y) (unigrams, bigrams) =
    (addOne x unigrams, addOne (x,y) bigrams)

bigramProbSimple :: SimpleModel -> (String,String) -> Double
bigramProbSimple (unigrams, bigrams) (x,y) =
    let num = Map.findWithDefault 0 (x,y) bigrams in
    let denom = Map.findWithDefault 0 x unigrams in
    if denom == 0 then 0 else (fromIntegral num / fromIntegral denom)

trainTestSimple :: [String] -> [String] -> Double
trainTestSimple training test =
    let model = trainModel addBigramSimple training freshSimpleModel in
    probOfSents (bigramProbSimple model) test

-----------------------------------------------------------------
-- The length-based bigram model

-- You can change the definitions of these types if you wish.
data LengthCat = Long | Short deriving (Eq,Show,Ord)
type LengthModel = (Map.Map LengthCat Int, Map.Map (LengthCat,String) Int)

freshLengthModel :: LengthModel
freshLengthModel = (Map.empty, Map.empty)

-- user-defined utility function
getLengthCat :: String -> LengthCat
getLengthCat x = 
    if length x > 4 then Long else Short

addBigramByLength :: (String,String) -> LengthModel -> LengthModel
addBigramByLength (x,y) (unigrams, bigrams) =
    let cat = getLengthCat x in
    (addOne cat unigrams, addOne (cat,y) bigrams)

bigramProbByLength :: LengthModel -> (String,String) -> Double
bigramProbByLength (unigrams, bigrams) (x,y) =
    let cat = getLengthCat x in
    let num = Map.findWithDefault 0 (cat,y) bigrams in
    let denom = Map.findWithDefault 0 cat unigrams in
    if denom == 0 then 0 else (fromIntegral num / fromIntegral denom)

trainTestLength :: [String] -> [String] -> Double
trainTestLength training test =
    let model = trainModel addBigramByLength training freshLengthModel in
    probOfSents (bigramProbByLength model) test

-----------------------------------------------------------------
-- The vowel-based bigram model

-- You can change the definitions of these types if you wish.
data VowelCat = Vowel | Cons deriving (Eq,Show,Ord)
type VowelModel = (Map.Map VowelCat Int, Map.Map (VowelCat,String) Int)

freshVowelModel :: VowelModel
freshVowelModel = (Map.empty, Map.empty)

-- user-defined utility function
getVowelCat :: String -> VowelCat
getVowelCat (x:xs) = 
    if elem x ['a','e','i','o','u'] then Vowel else Cons

addBigramByVowel :: (String,String) -> VowelModel -> VowelModel
addBigramByVowel (x,y) (unigrams, bigrams) =
    let cat = getVowelCat x in
    (addOne cat unigrams, addOne (cat,y) bigrams)

bigramProbByVowel :: VowelModel -> (String,String) -> Double
bigramProbByVowel (unigrams, bigrams) (x,y) =
    let cat = getVowelCat x in
    let num = Map.findWithDefault 0 (cat,y) bigrams in
    let denom = Map.findWithDefault 0 cat unigrams in
    if denom == 0 then 0 else (fromIntegral num / fromIntegral denom)

trainTestVowel :: [String] -> [String] -> Double
trainTestVowel training test =
    let model = trainModel addBigramByVowel training freshVowelModel in
    probOfSents (bigramProbByVowel model) test

-----------------------------------------------------------------
-- The interpolated model

interpolatedModel = (Map.empty, Map.empty)

bigramProbInterpolated :: Double -> LengthModel -> VowelModel -> (String,String) -> Double
bigramProbInterpolated weight lm vm (x,y) =
    (weight * bigramProbByLength lm (x,y)) + ((1 - weight) * (bigramProbByVowel vm (x,y)))

trainTestInterpolated :: Double -> [String] -> [String] -> Double
trainTestInterpolated weight training test =
    let length_model = trainModel addBigramByLength training freshLengthModel in
    let vowel_model = trainModel addBigramByVowel training freshVowelModel in
    probOfSents (bigramProbInterpolated weight length_model vowel_model) test
