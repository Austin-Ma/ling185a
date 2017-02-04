module RegExp where name

type Symbol = String

data RegExp = Sngtn String
            | Union RegExp RegExp
            | Concat RegExp RegExp
            | Star RegExp RegExp
            | Plus RegExp RegExp
            deriving (Eq,Show)

re1 :: RegExp
re1 = Star (Concat (Sngtn "hello") (Union (Sngtn "world") (Sngtn "there")))

-- generate :: RegExp -> [[String]]
match :: RegExp -> [String] -> Bool
match (Sngtn s) xs = xs == [s]
match (Union r1 r2) xs = match r1 xs || match r2 xs
match (Concat r1 r2) xs = ... (match r1 ...) ... (match r2 ... ) ...
match (Star r) xs = xs == [] || (match r ...)
match (Plus r) xs = match r xs || (match r ...)