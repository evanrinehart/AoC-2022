--{-# LANGUAGE ScopedTypeVariables #-}
module AStar where

import Data.IntMap (IntMap, (!)); import qualified Data.IntMap as IM
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
  , openSet  :: [(Int,p)]
  , cameFrom :: IntMap Int
  , decoder  :: IntMap p }
      deriving Show

makeGuts :: Ord p => PathSpace p -> p -> Guts p
makeGuts PathSpace{heuristic=h,encodePoint=en} start =
  let s = en start in
  Guts
    { fScores   = IM.singleton s 0
    , gScores   = IM.singleton s (h start)
    , openSet   = [(0,start)]
    , cameFrom  = IM.empty
    , decoder   = IM.singleton s start }

-- main loop
go :: Ord p => PathSpace p -> Guts p -> p -> [p]
go ps guts dest =
  let PathSpace{neighborhoodOf=nh,encodePoint=en} = ps   in
  let Guts{openSet=os,decoder=dc}  = guts in
  let (current,os')                = getCurrent guts in
  if current == dest
    then map (dc !) (reconstructPath guts (en current))
    else go2 ps dest current (nh current) guts{openSet=os'}

-- try all neighbors of current
go2 :: Ord p => PathSpace p -> p -> p -> [p] -> Guts p -> [p]
go2 space dest current []     guts = go space guts dest 
go2 space dest current (p:ps) guts = 
  let PathSpace{linkWeight=lw,heuristic=h,encodePoint=en} = space
      Guts{fScores=fs,gScores=gs,openSet=os,cameFrom=cf,decoder=dc} = guts
      x = en p
      currentx = en current
      tentativeG = getG guts currentx + lw current p
      fscore = tentativeG + h p
  in case tentativeG < getG guts x of
    False -> go2 space dest current ps guts
    True  -> go2 space dest current ps
      Guts { fScores  = IM.insert x fscore fs
           , gScores  = IM.insert x tentativeG gs
           , openSet  = pinsert (fscore,p) os
           , cameFrom = IM.insert x currentx cf
           , decoder  = IM.insert x p dc }

-- take point with lowest? F score from the openSet
getCurrent :: Guts p -> (p, [(Int,p)])
getCurrent Guts{openSet=(_,p):q} = (p,q)

-- G score of point
getG :: Guts p -> Int -> Int
getG guts x = fromMaybe maxBound (IM.lookup x (gScores guts))

reconstructPath :: Guts p -> Int -> [Int]
reconstructPath Guts{cameFrom=cf} end = go [] end where
  go xs x = case IM.lookup x cf of
    Nothing -> x:xs
    Just x' -> go (x:xs) x'

-- Priority insert
pinsert :: Ord a => a -> [a] -> [a]
pinsert x []     = [x]
pinsert x (y:ys) = if x <= y then x:y:ys else y : insert x ys

