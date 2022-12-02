module Main where

main = do
  theData <- loadData "input"
  print (sum (map (score . v1) theData))
  let hmm = map (ff . v2) theData
  print (sum (map score hmm))

ff (x,c) = let y = decrypt (x,c) in (x,y)

v1 (x,'X') = (x,Rock)
v1 (x,'Y') = (x,Paper)
v1 (x,'Z') = (x,Scissor)

v2 (x,'X') = (x,Lose)
v2 (x,'Y') = (x,Tie)
v2 (x,'Z') = (x,Win)

data RPS = Rock | Paper | Scissor deriving (Eq,Ord,Read,Show)
data Outcome = Win | Tie | Lose deriving (Eq,Ord,Read,Show)

pl (c1:' ':c2:_) = (f c1, c2) where
  f 'A' = Rock
  f 'B' = Paper
  f 'C' = Scissor

score :: (RPS,RPS) -> Int
score (a,b) = rps a b + g b where
  g Rock    = 1
  g Paper   = 2
  g Scissor = 3

xyz :: RPS -> Char
xyz Rock = 'X'
xyz Paper = 'Y'
xyz Scissor = 'Z'

-- in question 2, which move to make to get the outcome
decrypt :: (RPS,Outcome) -> RPS
decrypt (a,b) = f a b where
  f Rock Win = Paper
  f Paper Win = Scissor
  f Scissor Win = Rock
  f x Tie = x
  f Rock Lose = Scissor
  f Paper Lose = Rock
  f Scissor Lose = Paper

rps = f where
  f Rock Paper = 6
  f Scissor Rock = 6
  f Paper Scissor = 6
  f Paper Rock = 0
  f Rock Scissor = 0
  f Scissor Paper = 0
  f _ _ = 3

loadData :: FilePath -> IO [(RPS,Char)]
loadData = fmap f . readFile where
  f = map pl . lines
