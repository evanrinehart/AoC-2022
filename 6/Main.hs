module Main where

import Data.List (nub, splitAt, tails, findIndex)

unique :: [Char] -> Bool
unique cs = nub cs == cs

-- old
question1 :: Int -> [Char] -> Int
question1 n (a:b:c:d:more) =
  if unique [a,b,c,d] then n + 4 else question1 (n+1) (b:c:d:more)

-- old
question2 :: Int -> [Char] -> Int
question2 n cs =
  let (l,r) = splitAt 14 cs in
  if unique l then n + 14 else question2 (n+1) (tail l ++ r)

-- old
questionN :: Int -> [Char] -> Int
questionN size rawIn = go 0 rawIn where
  go n cs = let (l,r) = splitAt size cs in
            if unique l then n + size else go (n+1) (tail cs)

questionN2 size = fmap (+size) . findIndex unique . map (take size) . tails

main = do
  rawInput <- readFile "input"
  print (questionN 4  rawInput)
  print (questionN 14 rawInput)
  print (questionN2 4  rawInput)
  print (questionN2 14 rawInput)
