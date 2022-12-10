module Main where

data Ins = Noop | AddX Int deriving Show

simulate :: Int -> [Ins] -> [Int]
simulate x (Noop : more)    = x : simulate x more
simulate x (AddX dx : more) = x : x : simulate (x+dx) more
simulate x []               = [x]

strength :: [Int] -> [Int]
strength = zipWith (*) [1..]

answer1 = sum . take 6 . everyN 40 . drop 19 . strength . simulate 1

scanline :: [Int] -> [Char]
scanline xs = zipWith f xs [1..] where
  f str n = let i = (n-1) `mod` 40 in
            let x = str `div` n in
            if abs (x - i) <= 1 then '#' else '.'

answer2 = chunksOf 40 . scanline . strength . simulate 1

main = do
  ins <- loadData "input"
  print (answer1 ins)
  mapM_ putStrLn (answer2 ins)

loadData = fmap (map parseLine . lines) . readFile

parseLine :: String -> Ins
parseLine "noop" = Noop
parseLine ('a':'d':'d':'x':' ':rest) = let [(dx,_)] = reads rest in AddX dx

everyN :: Int -> [Int] -> [Int]
everyN n (x:xs) = x : everyN n (drop (n-1) xs)
everyN n []     = []

-- stolen from stack overflow
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = fxs : chunksOf n sxs
    where (fxs, sxs) = splitAt n xs
