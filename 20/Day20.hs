module Main where

import Data.List

main = do
  --values <- loadData "input"
  values <- fmap (map (* 811589153)) (loadData "input")
  let cmds = zip [0..] values
  let db0  = zip [0..] values
  --let values' = map snd $ iterate (mix cmds) db0 !! 1
  let values' = map snd $ iterate (mix cmds) db0 !! 10

  let values'' = dropWhile (/= 0) (cycle values')
  let (x:xs) = drop 1000 values''
  let (y:ys) = drop 1000 (x:xs)
  let (z:_)  = drop 1000 (y:ys)
  print [x,y,z]
  print (sum [x,y,z])

mix cmds db = loop db cmds

loop db [] = db
loop db ((id,n):more) = loop (moveBy id n db) more

loadData :: FilePath -> IO [Int]
loadData = fmap (map read . lines) . readFile

addLabels :: [Int] -> Db
addLabels = zip [0..]

type Id = Int
type Value = Int
type Db = [(Id,Value)]

indexOf :: Id -> Db -> Int
indexOf id db =
  let f (x,y) = x == id in
  case findIndex f db of
    Nothing -> error (show id ++ " not found")
    Just i  -> i

moveBy :: Id -> Int -> Db -> Db
moveBy id n db =
  let i  = indexOf id db
      i' = (i + n) `mod` 4999
  in case compare i i' of
    LT -> insertAt i' (id,n) (deleteAt i db)
    GT -> insertAt i' (id,n) (deleteAt i db)
    EQ -> db

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x []     = [x]
insertAt 0 x ys     = x : ys
insertAt i x (y:ys) = y : insertAt (i-1) x ys
insertAt i x []     = error "insertAt out of range"

deleteAt :: Int -> [a] -> [a]
deleteAt _ []     = error "deleteAt out of range"
deleteAt 0 (x:xs) = xs
deleteAt i (x:xs) = x : deleteAt (i-1) xs
