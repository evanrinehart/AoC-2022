module Main where

import qualified Data.Map as M; import Data.Map (Map, (!))

data Ex a =
  Leaf Int |
  Plus a a |
  Minus a a |
  Times a a |
  Divide a a
    deriving Show

type Name = String

loadData :: FilePath -> IO [(Name, Ex Name)]
loadData = fmap (map parseLine . lines) . readFile

parseLine :: String -> (Name, Ex Name)
parseLine str =
  let (name, ':':' ':rest) = splitAt 4 str in
  (name, parseEx rest)

parseEx str = case reads str of
  [(n,_)] -> Leaf n
  _ ->
    let [a,[b],c] = splitOn ' ' str in
    case b of
      '-' -> Minus a c
      '+' -> Plus a c
      '*' -> Times a c
      '/' -> Divide a c

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn z l@(x:xs)
  | x==z      = splitOn z xs
  | otherwise = let (h,t) = break (==z) l in h:(splitOn z t)

makeIndex :: [(Name,Ex Name)] -> Map Name (Ex Name)
makeIndex = M.fromList

eval :: Map Name (Ex Name) -> Ex Name -> Int
eval table (Leaf i) = i
eval table (Plus n1 n2) = eval table (table ! n1) + eval table (table ! n2)
eval table (Minus n1 n2) = eval table (table ! n1) - eval table (table ! n2)
eval table (Times n1 n2) = eval table (table ! n1) * eval table (table ! n2)
eval table (Divide n1 n2) = eval table (table ! n1) `div` eval table (table ! n2)

main = do
  table <- fmap makeIndex (loadData "input")
  print (eval table (table ! "root"))
  pure ()
