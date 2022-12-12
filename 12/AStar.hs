module AStar where

import Data.Map (Map); import qualified Data.Map as M
import Data.Set (Set); import qualified Data.Set as S
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
  { fScores  :: Map p Int
  , gScores  :: Map p Int
  , openSet  :: Set p
  , cameFrom :: Map p p }
      deriving Show

makeGuts :: Ord p => PathSpace p -> p -> Guts p
makeGuts PathSpace{heuristic=h} start = Guts
  { fScores   = M.singleton start 0
  , gScores   = M.singleton start (h start)
  , openSet   = S.singleton start
  , cameFrom  = M.empty }

-- main loop
go :: Ord p => PathSpace p -> Guts p -> p -> [p]
go ps guts dest =
  let PathSpace{neighborhoodOf=nh} = ps   in
  let Guts{openSet=os}             = guts in
  case getCurrent guts of
    Nothing      -> error "aStar: open set is empty (bug)"
    Just current ->
      if current == dest
        then reconstructPath guts current
        else let os' = S.delete current os
             in go2 ps dest current (nh current) guts{openSet=os'}

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
      Guts { fScores  = M.insert x (tentativeG + h x) fs
           , gScores  = M.insert x tentativeG gs
           , openSet  = S.insert x os
           , cameFrom = M.insert x current cf }

-- select best known point
getCurrent :: Ord p => Guts p -> Maybe p
getCurrent Guts{openSet=os, fScores=fs} = out where
  out  = (listToMaybe . sortBy (comparing score) . S.toList) os
  score p = case M.lookup p fs of
    Nothing    -> error "aStar: point not in fScores (bug)"
    Just score -> score

-- G score of point
getG :: Ord p => Guts p -> p -> Int
getG guts p = fromMaybe maxBound (M.lookup p (gScores guts))

reconstructPath :: Ord p => Guts p -> p -> [p]
reconstructPath Guts{cameFrom=cf} end = go [] end where
  go ps p = case M.lookup p cf of
    Nothing -> p:ps
    Just p' -> go (p:ps) p'
