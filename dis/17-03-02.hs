bigramProb :: (Str,Str) -> Float
allWords :: [String]

probAtPos :: Int -> String -> Float
probAtPos 0 s = bigramProb("<s>", s)
probAtPos n s = sum (map (lw - probPos(n-1) w * bigramProb(w, s)) allwords)
-- for every word, get prob of word coming before current word for each word