module AStar where

import Data.Map (Map, (!)); import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Ord

-- * API

data PathSpace p = PathSpace
  { linkWeight     :: p -> p -> Int
  , heuristic      :: p -> Int
  , neighborhoodOf :: p -> [p] }
  
findPathFromTo :: Ord p => PathSpace p -> p -> p -> [p]
findPathFromTo ps start end = go ps (makeGuts ps start) end

-- * Guts

data Guts p = Guts
  { fScores  :: Map p Int
  , gScores  :: Map p Int
  , openSet  :: [(Int,p)]
  , cameFrom :: Map p p }
      deriving Show

makeGuts :: PathSpace p -> p -> Guts p
makeGuts PathSpace{heuristic=h} start = Guts
  { fScores   = M.singleton start 0
  , gScores   = M.singleton start (h start)
  , openSet   = [(0,start)]
  , cameFrom  = M.empty }

-- main loop
go :: Ord p => PathSpace p -> Guts p -> p -> [p]
go ps guts dest =
  let nh         = neighborhoodOf ps in
  let (here,os') = takeBest guts in
  if here == dest
    then reconstructPath guts here
    else go2 ps dest here (nh here) guts{openSet=os'}

-- try all neighbors of current
go2 :: Ord p => PathSpace p -> p -> p -> [p] -> Guts p -> [p]
go2 space dest from []     guts = go space guts dest 
go2 space dest from (p:ps) guts = 
  let PathSpace{linkWeight=lw,heuristic=h} = space
      Guts{fScores=fs,gScores=gs,openSet=os,cameFrom=cf} = guts
      tentativeG = getG guts from + lw from p
      fscore = tentativeG + h p
  in case tentativeG < getG guts p of
    False -> go2 space dest from ps guts
    True  -> go2 space dest from ps
      Guts { fScores  = M.insert p fscore fs
           , gScores  = M.insert p tentativeG gs
           , openSet  = pinsert (fscore,p) os
           , cameFrom = M.insert p from cf }

-- take point with lowest? F score from the openSet
takeBest :: Guts p -> (p, [(Int,p)])
takeBest Guts{openSet=(_,p):q} = (p,q)

-- G score of point
getG :: Ord p => Guts p -> p -> Int
getG guts x = fromMaybe maxBound (M.lookup x (gScores guts))

reconstructPath :: Ord p => Guts p -> p -> [p]
reconstructPath Guts{cameFrom=cf} end = go [] end where
  go xs x = case M.lookup x cf of
    Nothing -> x:xs
    Just x' -> go (x:xs) x'

-- Priority insert
pinsert :: Ord a => a -> [a] -> [a]
pinsert x []     = [x]
pinsert x (y:ys) = if x <= y then x:y:ys else y : insert x ys
