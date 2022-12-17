{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Char
import qualified Data.IntMap as IM
import Data.IntMap ((!))
import Data.List
import Data.Ord
import Data.Function

import Control.Parallel.Strategies

import AStar (findPathFromTo, PathSpace(..))

newtype Node = Node { unNode :: Int } deriving (Eq,Ord,Read)

instance Show Node where
  show = (\(x,y) -> [x,y]) . decodeNode

type Triple = (Node,Int,[Node])

encodeNode :: Char -> Char -> Node
encodeNode c1 c2 = Node (j*26 + i) where
  j = ord c1 - base
  i = ord c2 - base
  base = ord 'A'

decodeNode :: Node -> (Char,Char)
decodeNode (Node n) = (chr (a+base), chr (b+base)) where
  (a,b) = n `divMod` 26
  base = ord 'A'

makeNeighborhood :: [Triple] -> Node -> [Node]
makeNeighborhood triples here = m ! (unNode here) where
  m               = fmap nub (g links)
  links           = concatMap f triples
  f (from, _, ns) = ns >>= \to -> [(unNode from,to),(unNode to,from)] 
  g               = IM.fromList . map h . groupBy ((==) `on` fst) . sortBy (comparing fst)
  h xs            = let node1 = fst (head xs) in (node1, map snd xs)

makeScoreIndex :: [Triple] -> Node -> Int
makeScoreIndex triples here = m ! (unNode here) where
  m              = IM.fromList (map f triples)
  f (x,y,z) = (unNode x,y)

coreNodes :: [Triple] -> [Node]
coreNodes = map f . filter g where
  g (_,x,_) = x > 0
  f (n,_,_) = n

takes :: [a] -> [(a,[a])]
takes [] = []
takes xs = zipWith f (init (inits xs)) (init (tails xs)) where
  f before (a:fter) = (a, before++fter)
  
maxPoints :: (Node -> Int) -> (Node -> Node -> Int) -> [Node] -> Node -> Int -> Int -> Int
maxPoints valueOf ruler urn here fuel accum = maximum (parMap rpar f (takes urn)) where
  f (next,urn') =
    let cost = ruler here next + 1 in
    let points = valueOf next in
    if cost <= fuel
      then let !accum' = accum + points in
           let !fuel'  = fuel - cost in
           maxPoints valueOf ruler urn' next fuel' accum'
      else accum
  
makeRuler :: [Triple] -> Node -> Node -> Int
makeRuler triples from to = m ! unNode from ! unNode to where
  getNH = makeNeighborhood triples
  astar x y = length (findPathFromTo space x y) - 1
  nodes = map (\(x,y,z) -> x) triples

  m = IM.fromList (map f nodes)
  f n1 = let others = filter (/= n1) nodes
             m' = IM.fromList (map (g n1) others)
         in (unNode n1, m')
  g n1 n2 = (unNode n2, astar n1 n2)

  space = PathSpace
    { linkWeight = const (const 1)
    , heuristic  = const 1
    , neighborhoodOf = getNH }
  
main = do
  triples <- loadData "input"
  let cores = coreNodes triples
  let ruler = makeRuler triples
  let valueOf = makeScoreIndex triples
  print $ maxPoints valueOf ruler cores aa 15 0
  pure ()

aa = encodeNode 'A' 'A'

gut c = drop 1 . dropWhile (/= c)

loadData = fmap (map parseLine . lines) . readFile

parseLine :: String -> (Node, Int, [Node])
parseLine str =
  let (here, rest)    = parseNode (gut ' ' str)
      [(rate, rest')] = reads (gut '=' rest)
      peers           = (map (toNode . dropWhile (==' ')) . splitOn ',' . dropWhile (not . isUpper)) rest'
  in (here, rate, peers)

toNode (c1:c2:_) = encodeNode c1 c2
toNode (c1:_)    = encodeNode c1 (error "wtf")
toNode []        = error "wtf"

parseNode :: String -> (Node, String)
parseNode (c1:c2:more) = (encodeNode c1 c2, more)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn z l@(x:xs)
  | x==z      = splitOn z xs
  | otherwise = let (h,t) = break (==z) l in h:(splitOn z t)
