module Main where

import Data.List
import Data.Char

data Sack = Sack [Char] [Char]
  deriving Show

loadSack :: String -> Sack
loadSack str = Sack l r where
  (l,r) = splitAt n str
  n     = length str `div` 2

loadData :: FilePath -> IO [Sack]
loadData = fmap (map loadSack . lines) . readFile

commonItem :: Sack -> Char
commonItem (Sack l r) = head (intersect l r)

commonItem3 :: [Sack] -> Char
commonItem3 [Sack l1 r1, Sack l2 r2, Sack l3 r3] =
  head (foldl1 intersect [l1++r1, l2++r2, l3++r3])

priority :: Char -> Int
priority c
  | c < 'a'   = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = fxs : chunksOf n sxs
    where (fxs, sxs) = splitAt n xs

main = do
  sacks <- loadData "input"
  print (sum (map (priority . commonItem) sacks))
  let groups = chunksOf 3 sacks
  print (sum (map (priority . commonItem3) groups))
