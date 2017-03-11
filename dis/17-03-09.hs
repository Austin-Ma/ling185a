easy example
      "a" "d"
    S1  S2  S3
10   1   0   0
20   0 .08   0
30   0 .42   0
40   0   0 .41

forward algorithm: sum probabilities from previous states
verterbi algorithm: w/ multiple paths to emit a letter/sequence, which path is
the most likely?
uses induction (recursion?)
take max of previous column instead of sum
only diff is verterbi backtracks to get best sequence
hw: write "backtracking thing" (using forward algorithm)

more complicated example
      "a" "d"
    S1  S2  S3
10   1   0   0
20   0   0   0