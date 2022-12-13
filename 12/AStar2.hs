module AStar where

import Data.IntMap (IntMap, (!)); import qualified Data.IntMap as IM
import Data.Maybe
import Data.List
import Data.Ord
import Control.Monad.Reader
import Control.Monad.State

-- * API

type Node = Int

data PathSpace p = PathSpace
  { linkWeight     :: p -> p -> Int
  , heuristic      :: p -> Int
  , neighborhoodOf :: p -> [p] }
  
findPathFromTo :: PathSpace Node -> Node -> Node -> [Node]
findPathFromTo ps start goal = evalState (runReaderT (outerLoop goal) ps) (makeGuts ps start)

-- * Guts

data Guts = Guts
  { fScores  :: IntMap Score
  , gScores  :: IntMap Score
  , openSet  :: [(Int,Node)]
  , cameFrom :: IntMap Int }
      deriving Show

makeGuts :: PathSpace Node -> Node -> Guts
makeGuts PathSpace{heuristic=h} start =
  Guts
    { fScores   = IM.singleton start (Score 0)
    , gScores   = IM.singleton start (Score (h start))
    , openSet   = [(0,start)]
    , cameFrom  = IM.empty }

type AStar p a = ReaderT (PathSpace p) (State Guts) a

outerLoop :: Node -> AStar Node [Node]
outerLoop goal = do
  here <- takeBestFromOpenSet
  if here == goal
    then reconstructPathFrom here
    else do
      peers <- getNeighborsOf here
      mapM_ (innerLoop here) peers
      outerLoop goal

innerLoop :: Node -> Node -> AStar Node ()
innerLoop from to = do
  someG <- withGW (\g w -> g from + w from to)
  myG   <- withGW (\g _ -> g to)
  when (someG < myG) $ do
    myF <- withH (\h -> myG + h to)
    markOpen to myF
    updateF to myF -- unnecessary?
    updateG to myG
    noteCameFrom to from

takeBestFromOpenSet :: AStar Node Node
takeBestFromOpenSet = state $ \guts ->
  let ((_,p):more) = openSet guts in
  (p, guts{openSet=more})

getNeighborsOf :: p -> AStar p [p]
getNeighborsOf p = do
  nh <- asks neighborhoodOf
  pure (nh p)

markOpen :: Node -> Score -> AStar Node ()
markOpen p score = modify (onOpenSet (pinsert (fromScore score,p)))

updateF :: Node -> Score -> AStar Node ()
updateF name score = modify (onFScores (IM.insert name score))

updateG :: Node -> Score -> AStar Node ()
updateG name score = modify (onGScores (IM.insert name score))

onOpenSet f guts = guts { openSet = f (openSet guts) }
onFScores f guts = guts { fScores = f (fScores guts) }
onGScores f guts = guts { gScores = f (gScores guts) }
onCameFrom f guts = guts { cameFrom = f (cameFrom guts) }

noteCameFrom :: Int -> Int -> AStar p ()
noteCameFrom to from = modify (onCameFrom (IM.insert to from))

reconstructPathFrom :: Node -> AStar Node [Node]
reconstructPathFrom end = wrapper where
  wrapper = do
    cf <- gets cameFrom
    pure (go cf [] end)
  go cf xs x = case IM.lookup x cf of
    Nothing -> x:xs
    Just x' -> go cf (x:xs) x'

withGW :: ((Node -> Score) -> (Node -> Node -> Score) -> Score) -> AStar Node Score
withGW f = do
  gtable <- gets gScores
  let g i = fromMaybe infinity (IM.lookup i gtable)
  lw <- asks linkWeight
  let w p1 p2 = Score (lw p1 p2)
  pure (f g w)

withH :: ((p -> Score) -> Score) -> AStar p Score
withH f = do
  h <- fmap (Score .) (asks heuristic)
  pure (f h)

-- Priority insert
pinsert :: Ord a => a -> [a] -> [a]
pinsert x []     = [x]
pinsert x (y:ys) = if x <= y then x:y:ys else y : insert x ys



-- Int with infinity
newtype Score = Score { fromScore :: Int } deriving (Eq,Show,Read)

infinity = Score (-1)

maybeScore :: Maybe Int -> Score
maybeScore Nothing  = infinity
maybeScore (Just s) = Score s

instance Num Score where
  Score m + Score n
    | m < 0 || n < 0 = Score (-1)
    | otherwise      = Score (m + n)

instance Ord Score where
  Score m < Score n
    | m < 0     = False
    | n < 0     = True
    | otherwise = m < n
