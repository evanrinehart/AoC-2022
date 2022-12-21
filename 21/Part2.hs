module Main where

import qualified Data.Map as M; import Data.Map (Map, (!))

data Ex a =
  Leaf Int |
  Plus a a |
  Minus a a |
  Times a a |
  Divide a a |
  X
    deriving Show

data T =
  N Int |
  Var Name |
  Human |
  Binop T Char T
    deriving Show

type Name = String

loadData :: FilePath -> IO [(Name, T)]
loadData = fmap (map parseLine . lines) . readFile

parseLine :: String -> (Name, T)
parseLine str =
  let (name, ':':' ':rest) = splitAt 4 str in
  (name, parseT rest)

parseT str = case reads str of
  [(n,_)] -> N n
  _ ->
    let [a,[b],c] = splitOn ' ' str in
    let f "humn" = Human; f str = Var str in
    Binop (f a) b (f c)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn z l@(x:xs)
  | x==z      = splitOn z xs
  | otherwise = let (h,t) = break (==z) l in h:(splitOn z t)

makeIndex :: [(Name,T)] -> Map Name T
makeIndex = M.fromList


simp :: Map Name T -> T -> T
simp table = f where
  f (N i) = N i
  f (Var name) = f (table ! name)
  f Human = Human
  f (Binop Human '+' Human) = Binop Human '*' (N 2)
  f (Binop Human '-' Human) = N 0
  f (Binop Human '*' Human) = Binop Human '^' (N 2)
  f (Binop Human '/' Human) = N 1
  f (Binop Human o x) = Binop Human o (f x)
  f (Binop x o Human) = Binop (f x) o Human
  f (Binop x o y) = tryCompute o (f x) (f y)

tryCompute :: Char -> T -> T -> T
tryCompute o (N x) (N y) = N (compute o x y)
tryCompute o t1 t2       = Binop t1 o t2

compute :: Char -> Int -> Int -> Int
compute '+' = (+)
compute '-' = (-)
compute '*' = (*)
compute '/' = div

pretty :: T -> String
pretty (N i) = show i
pretty (Binop t1 o t2) = "(" ++ pretty t1 ++ " " ++ [o] ++ pretty t2 ++ ")"
pretty (Var name) = show name
pretty Human = "x"

algebra :: T -> T
algebra (Binop (Binop t1 '+' t2) '*' t3) = Binop (Binop t3 '*' t1) '+' (Binop t3 '*' t2)

main = do
  table <- fmap makeIndex (loadData "input")
  let Binop (Var n1) _ (Var n2) = table ! "root"
  let tl = table ! n1
  let tr = table ! n2
  print (pretty $ simp table (simp table tl))
  --mapM_ print (M.toList table)
  pure ()
