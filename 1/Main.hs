module Main where

import Data.List

main = do
  theData <- loadData "input"
  let highScores = sortElves theData
  let topThree = take 3 highScores
  print (head topThree)
  print (sum topThree)

sortElves :: [[Int]] -> [Int]
sortElves = reverse . sort . map sum

loadData :: FilePath -> IO [[Int]]
loadData = fmap f . readFile where
  f = map (map read) . splitOn "" . lines

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn z l@(x:xs)
  | x==z      = splitOn z xs
  | otherwise = let (h,t) = break (==z) l in h:(splitOn z t)

{-
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
  | f x = splitOn f xs
  | otherwise = let (h,t) = break f l in h:(splitOn f t)
-}
