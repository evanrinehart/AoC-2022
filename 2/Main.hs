module Main where

data RPS     = Rock | Paper | Scissor deriving (Eq,Ord,Read,Show)
data Outcome = Win | Draw | Lose deriving (Eq,Ord,Read,Show)

main = do
  c1raw <- loadColumn1 "input"
  c2raw <- loadColumn2 "input"

  let c1 = map unABC c1raw

  let c2 = map unXYZ c2raw
  print (sum (zipWith score c1 c2))

  let c2' = map unXYZ' c2raw
  let c3  = zipWith decrypt c1 c2'
  print (sum (zipWith score c1 c3))

unABC 'A' = Rock
unABC 'B' = Paper
unABC 'C' = Scissor

unXYZ 'X' = Rock
unXYZ 'Y' = Paper
unXYZ 'Z' = Scissor

unXYZ' 'X' = Lose
unXYZ' 'Y' = Draw
unXYZ' 'Z' = Win

score :: RPS -> RPS -> Int
score a b = rps a b + g b where
  g Rock    = 1
  g Paper   = 2
  g Scissor = 3

rps = f where
  f Rock    Paper   = 6
  f Scissor Rock    = 6
  f Paper   Scissor = 6
  f Paper   Rock    = 0
  f Rock    Scissor = 0
  f Scissor Paper   = 0
  f _       _       = 3

decrypt :: RPS -> Outcome -> RPS
decrypt x       Draw  = x
decrypt Rock    Win  = Paper
decrypt Paper   Win  = Scissor
decrypt Scissor Win  = Rock
decrypt Rock    Lose = Scissor
decrypt Paper   Lose = Rock
decrypt Scissor Lose = Paper

loadColumn1 :: FilePath -> IO [Char]
loadColumn1 = fmap (map head . lines) . readFile

loadColumn2 :: FilePath -> IO [Char]
loadColumn2 = fmap (map f . lines) . readFile where
  f (_:_:c:_) = c
