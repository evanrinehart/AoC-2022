module AStar where

import Data.IntMap (IntMap); import qualified Data.IntMap as IM
import Data.Maybe
import Data.List
import Data.Ord

-- * API

data PathSpace p = PathSpace
  { linkWeight     :: p -> p -> Int
  , heuristic      :: p -> Int
  , neighborhoodOf :: p -> [p]
  , encodePoint    :: p -> Int }
  
findPathFromTo :: Ord p => PathSpace p -> p -> p -> [p]
findPathFromTo ps start end = go ps (makeGuts ps start) end

-- * Guts

data Guts p = Guts
  { fScores  :: IntMap Int
  , gScores  :: IntMap Int
  , openSet  :: OpenSet p
  , cameFrom :: IntMap p }
      deriving Show

type OpenSet a = [(Int,a)]

makeGuts :: Ord p => PathSpace p -> p -> Guts p
makeGuts PathSpace{heuristic=h,encodePoint=en} start =
  let s = en start in
  Guts
    { fScores   = IM.singleton s 0
    , gScores   = IM.singleton s (h start)
    , openSet   = [(0,start)]
    , cameFrom  = IM.empty }

-- main loop
go :: Ord p => PathSpace p -> Guts p -> p -> [p]
go ps guts dest =
  let PathSpace{neighborhoodOf=nh} = ps   in
  let Guts{openSet=os}             = guts in
  let (current,os')                = getCurrent guts in
  if current == dest
    then reconstructPath guts current
    else go2 ps dest current (nh current) guts{openSet=os'}

-- try all neighbors of current
go2 :: Ord p => PathSpace p -> p -> p -> [p] -> Guts p -> [p]
go2 ps dest current []     guts = go ps guts dest 
go2 ps dest current (x:xs) guts = 
  let PathSpace{linkWeight=lw,heuristic=h} = ps in
  let Guts{fScores=fs,gScores=gs,openSet=os,cameFrom=cf} = guts in
  let tentativeG = getG guts current + lw current x in
  case tentativeG < getG guts x of
    False -> go2 ps dest current xs guts
    True  -> go2 ps dest current xs
      Guts { fScores  = M.insert x fscore fs
           , gScores  = M.insert x tentativeG gs
           , openSet  = pinsert (fscore,x) os
           , cameFrom = M.insert x current cf } where fscore = tentativeG + h x

-- take point with lowest? F score from the openSet
getCurrent :: Ord p => Guts p -> (p, OpenSet p)
getCurrent Guts{openSet=(_,p):q} = (p,q)

-- G score of point
getG :: Ord p => Guts p -> p -> Int
getG guts p = fromMaybe maxBound (M.lookup p (gScores guts))

reconstructPath :: Ord p => (p -> Int) -> Guts p -> p -> [p]
reconstructPath en Guts{cameFrom=cf} end = go [] end where
  go ps p = case IM.lookup (en p) cf of
    Nothing -> p:ps
    Just p' -> go (p:ps) p'

-- Priority insert
pinsert :: Ord a => a -> [a] -> [a]
pinsert x []     = [x]
pinsert x (y:ys) = if x <= y then x:y:ys else y : insert x ys

