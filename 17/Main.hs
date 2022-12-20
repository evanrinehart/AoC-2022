module Main where

import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Bits ((.|.))

default (Int)

type Point = (Int,Int)
type Piece = [Point]
data Obj = Obj Point Piece
  deriving Show
data Grid = Grid
  { topOf    :: !Int
  , solidSet :: !(Set (Int,Int)) }
    deriving Show

cross  = [(0,1),(1,1),(2,1),(1,2),(1,0)]
flat   = [(0,0),(1,0),(2,0),(3,0)]
post   = [(0,0),(0,1),(0,2),(0,3)]
corner = [(0,0),(1,0),(2,0),(2,1),(2,2)]
block  = [(0,0),(1,0),(0,1),(1,1)]

points :: Obj -> [Point]
points (Obj (x,y) points) = map f points where
  f (p0,p1) = (p0+x, p1+y)

overlaps :: Obj -> Grid -> Bool
overlaps obj (Grid _ s) = any f (points obj) where
  f p@(x,y) = x < 0 || 6 < x || y < 0 || p `S.member` s

rightmost :: Obj -> Int
rightmost (Obj (x,y) piece) = maximum (map fst piece) + x

leftmost :: Obj -> Int
leftmost (Obj (x,y) piece)  = minimum (map fst piece) + x

left :: Obj -> Obj
left o@(Obj (x,y) piece)
  | leftmost o > 0 = Obj (x-1,y) piece
  | otherwise      = o

right :: Obj -> Obj
right o@(Obj (x,y) piece)
  | rightmost o < 6 = Obj (x+1,y) piece
  | otherwise       = o

down :: Obj -> Maybe Obj
down (Obj (x,y) piece)
  | y > 0     = Just (Obj (x,y-1) piece)
  | otherwise = Nothing

commit :: Grid -> Obj -> Grid
commit (Grid top s) obj = Grid top' s' where
  ps   = points obj
  s'   = foldl' (flip S.insert) s (points obj)
  top' = foldl1' max (top : map snd ps)

spawn :: Piece -> Grid -> Obj
spawn piece (Grid top _) = Obj (2,top + 4) piece

juke :: Char -> Obj -> Obj
juke '>' = right
juke '<' = left
juke c = error ("unexpected " ++ show (ord c))

simulate :: [Char] -> Obj -> Grid -> (Grid,[Char])
simulate commands start grid = side commands start where
  side (c:cs) obj =
    let obj' = juke c obj in
    if overlaps obj' grid
      then fall cs obj
      else fall cs obj'
  fall cs obj = case down obj of
    Nothing -> (commit grid obj, cs)
    Just obj'
      | overlaps obj' grid -> (commit grid obj, cs)
      | otherwise          -> side cs obj'

encodeLine :: Set (Int,Int) -> Int -> Int
encodeLine s j = g (map f [0..6]) where
  f i = if S.member (i,j) s then 2^i else 0
  g = foldl1' (.|.)

main = do
  rawInput <- fmap (filter (/='\n')) (readFile "input")
  let commands = cycle rawInput
  --let commands = cycle ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
  let pieces   = cycle [flat, cross, corner, post, block]
  let grid     = Grid (-1) S.empty
  --mapM_ print $ take 4 $ loop commands pieces (Grid (-1) S.empty)
  --print (1 + topOf (loop commands pieces (Grid (-1) S.empty) !! 2022))
  let grid' = loop commands pieces grid !! 9
  --mapM_ print (visual grid')
  print (1 + topOf grid')

loop cs (p:ieces) grid = 
  let (grid', cs') = simulate cs (spawn p grid) grid in
  grid' : loop cs' ieces grid'

visualLine :: Grid -> Int -> String
visualLine (Grid _ s) j = map (g . f) [0..6] where
  f i = S.member (i,j) s
  g False = '.'
  g True  = '#'

visual :: Grid -> [String]
visual grid@(Grid top s) = map (visualLine grid) [top, top-1 .. 0]
