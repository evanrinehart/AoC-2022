{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Char
--import qualified Data.IntMap as IM
--import Data.IntMap ((!))
import Data.List
import Data.Ord
import Data.Function

import qualified Data.Map as M
import Data.Map (Map,(!))

import Control.DeepSeq

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
  
main = do
  triples <- loadData "input"
  let ruler  = makeRuler triples
  let flowOf = makeFlowTable triples
  let meter = flowOf
  let urn    = reverse $ sortBy (comparing flowOf) (coreNodes triples)
  let start  = Game1 aa 0 30 urn


  --print (bounds ruler meter start, start)

  --let f = maximum . map scoreUB
  let g = length . filter (\Game1{oneUrn=urn} -> urn==[])

  let pile = take 10 $ iterate (step1 ruler meter) [start]
  mapM_ (print . length) $ pile
  mapM_ print (last pile)
  --let pile = iterate (step1 ruler meter) [start] !! 11
  --mapM_ print pile
  --print (bounds ruler meter g1)
  --print (bounds ruler meter g2)
  
  
  --splitGame



pruneBy :: (a -> Int) -> (a -> Int) -> [a] -> [a]
pruneBy l r xs =
  let cutoff = maximum (map l xs)
  in filter ((>= cutoff) . r) xs

{-
step :: Game g => [g] -> [g]
step pile = pruneBy l r $ concatMap f pile where
  l = scoreLB
  r = scoreUB
  f game = case split game of
    []    -> [game]
    other -> other
-}

type Ruler = Node -> Node -> Int
type Gauge = Node -> Int

step1 :: Ruler -> Gauge -> [Game1] -> [Game1]
step1 ruler meter pile = pruneBy l r $ concatMap f pile where
  l = scoreLB1 ruler meter
  r = scoreUB1 ruler meter
  f game = case split1 ruler meter game of
    []    -> [game]
    other -> other

data Game1 = Game1
  { onePointer :: Node
  , oneScore   :: Int
  , oneTimer   :: Int
  , oneUrn     :: [Node] }

instance Show Game1 where
  show (Game1 ptr s t urn) =
    "{ptr=" ++ show ptr ++
    ",score=" ++ show s ++
    ",time=" ++ show t ++
    ",urn=" ++ show urn ++ "}"

{-
splitGame :: Game1 -> [Game1]
splitGame (Game1 ruler flowOf ptr s t urn) = do
  next <- urn
  let urn' = delete next urn
  let d    = ruler ptr next
  let t'   = t - d - 1
  if t' < 0
    then [] -- out of time
    else do
      let rate = flowOf next
      let s'   = s + rate * t'
      [Game1 ruler flowOf next s' t' urn']
-}

-- assume this move is valid
move1 :: Ruler -> Gauge -> Game1 -> Node -> Game1
move1 ruler meter (Game1 ptr s t urn) next = cleanUrn ruler g' where
  !urn' = force (delete next urn)
  d    = ruler ptr next
  t'   = t - d - 1
  rate = meter next
  !s'   = s + rate * t'
  g'   = Game1 next s' t' urn'

-- same as move but distance cost is reduced to 1
cheat1 :: Ruler -> Gauge -> Game1 -> Node -> Game1
cheat1 ruler meter (Game1 ptr s t urn) next = cleanUrn ruler g' where
  !urn' = force (delete next urn)
  t'   = t - 2
  rate = meter next
  !s'   = s + rate * t'
  g'   = Game1 next s' t' urn'

validMoves1 :: Ruler -> Game1 -> [Node]
validMoves1 ruler (Game1 ptr s t urn) = filter f urn where
  f node = ruler ptr node + 1 <= t

cleanUrn :: Ruler -> Game1 -> Game1
cleanUrn ruler g = g{oneUrn=validMoves1 ruler g}

split1 :: Ruler -> Gauge -> Game1 -> [Game1]
split1 ruler gauge   Game1{oneUrn=[]}    = []
split1 ruler gauge g@Game1{oneUrn=moves} = map (move1 ruler gauge g) moves

-- game that is over has actual score as lower bound
scoreLB1 :: Ruler -> Gauge -> Game1 -> Int
scoreLB1 _ _ (Game1 _ s _ _) = s
--scoreLB1 ruler meter   (Game1 _ s t []) = s
--scoreLB1 ruler meter   (Game1 _ s 0 _ ) = s
--scoreLB1 ruler meter g@(Game1 _ s _ (m:oves) ) = scoreLB1 ruler meter (move1 ruler meter g m)
  
scoreUB1 :: Ruler -> Gauge -> Game1 -> Int
scoreUB1 ruler meter   (Game1 _ s t []) = s
scoreUB1 ruler meter   (Game1 _ s 0 _ ) = s
scoreUB1 ruler meter g@(Game1 _ s t (m:oves)) = scoreUB1 ruler meter (cheat1 ruler meter g m)



{-
scoreLB :: Ruler -> FlowMeter -> Game -> Int
--scoreLB _ _ (Game _ s _ _) = s
scoreLB ruler meter (Game ptr s t [])   = s
scoreLB ruler meter (Game ptr s 0 urn)  = s
scoreLB ruler meter (Game ptr s t (next:urn')) = 
  let d  = ruler ptr next in
  let t' = t - d - 1 in
  if t' < 0
    then s -- out of time
    else
      let rate = meter next in
      let s'   = s + rate * t' in
      scoreLB ruler meter (Game next s' t' urn')


scoreUB :: Ruler -> FlowMeter -> Game -> Int
scoreUB ruler flowOf (Game ptr s t [])  = s
scoreUB ruler flowOf (Game ptr s 0 urn) = s
scoreUB ruler flowOf (Game ptr s t (next:urn')) =
  let t' = t - 2 in
  if t' < 0
    then s -- out of time
    else
      let rate = flowOf next in
      let s'   = s + rate * t' in
      scoreUB ruler flowOf (Game next s' t' urn')

search :: Ruler -> FlowMeter -> Game -> Game
search ruler meter start = go start (bounds start) []

loop ruler meter pile =
  get bounds of everything in the pile
  get thing with max lower bound
  if it's greater than max upper bound of the others, it's winning
  
  moves = splitGame ruler meter start
  if each move
  if (lb,ub) of any move < best lb ub, discard
  if (lb,ub) of any move > best lb ub, replace best
  else


data Player = Stuck | Ready | Traveling Node Int | Activating Node |

data Game2 = Game2
  { gameP1    :: Player
  , gameP2    :: Player
  , gameScore :: Int
  , gameTimer :: Int
  , gameUrn   :: [Node] }
      deriving Show
-}

hmm = map f ["XZ","ED","RW","JY","DI","VF","AF"] where
  f [c1,c2] = encodeNode c1 c2


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



makeNeighborhood :: [Triple] -> Node -> [Node]
makeNeighborhood triples here = m ! (unNode here) where
  m               = fmap nub (g links)
  links           = concatMap f triples
  f (from, _, ns) = ns >>= \to -> [(unNode from,to),(unNode to,from)] 
  g               = M.fromList . map h . groupBy ((==) `on` fst) . sortBy (comparing fst)
  h xs            = let node1 = fst (head xs) in (node1, map snd xs)

makeFlowTable :: [Triple] -> Node -> Int
makeFlowTable triples here = m ! here where
  m         = M.fromList (map f triples)
  f (x,y,z) = (x,y)

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
makeRuler triples from to = m ! from ! to where
  getNH = makeNeighborhood triples
  astar x y = length (findPathFromTo space x y) - 1
  nodes = map (\(x,y,z) -> x) triples

  m = M.fromList (map f nodes)
  f n1 = let others = filter (/= n1) nodes
             m'     = M.fromList (map (g n1) others)
         in (n1, m')
  g n1 n2 = (n2, astar n1 n2)

  space = PathSpace
    { linkWeight = const (const 1)
    , heuristic  = const 1
    , neighborhoodOf = getNH }
