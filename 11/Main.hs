module Main where

import Prelude hiding (round)
import Control.Monad.RWS
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as M

brains = 
  [Brain (OldTimes 3)  (Test 2  1 4)   -- 0
  ,Brain (OldTimes 19) (Test 7  3 5)   -- 1
  ,Brain (OldPlus 2)   (Test 11 4 0)   -- 2
  ,Brain (OldSquared)  (Test 19 7 6)   -- 3
  ,Brain (OldPlus 8)   (Test 3  5 1)   -- 4
  ,Brain (OldPlus 6)   (Test 5  3 6)   -- 5
  ,Brain (OldPlus 7)   (Test 17 7 2)   -- 6
  ,Brain (OldPlus 4)   (Test 13 2 0)]  -- 7

startingInventories :: Map Int [Int]
startingInventories = M.fromList
  [(0, [66, 59, 64, 51])
  ,(1, [67, 61])
  ,(2, [86, 93, 80, 70, 71, 81, 56])
  ,(3, [94])
  ,(4, [71, 92, 64])
  ,(5, [58, 81, 92, 75, 56])
  ,(6, [82, 98, 77, 94, 86, 81])
  ,(7, [54, 95, 70, 93, 88, 93, 63, 50])]

main = do
  let divisors = map divisorOf brains
  let inv1 = startingInventories
  let inv2 = M.map (map (toRidiculous divisors)) startingInventories
  print (answer brains 20    inv1)
  print (answer brains 10000 inv2)

-- Run n rounds with given brains and starting worry list
answer :: Worry n => [Brain] -> Int -> Map Int [n] -> Int
answer brains n inv =
  let (_, _, Tally counts) = runRWS (replicateM n round) brains inv
      [n1,n2]              = (take 2 . reverse . sort . M.elems) counts
  in n1 * n2

round :: Worry n => Business n ()
round = ask >>= \brains -> mapM_ (uncurry monkey) (zip [0..] brains)

monkey :: Worry n => Int -> Brain -> Business n ()
monkey i (Brain op (Test d t1 t2)) = do
  items <- gets (! i)
  forM_ items $ \lvl -> do
    let lvl'   = relieve (applyOp op lvl)
    let target = if d `divides` lvl' then t1 else t2
    throwTo target lvl'
    incrementTally i 
  modify (M.insert i [])

throwTo :: Int -> n -> Business n ()
throwTo target item = modify (M.adjust (++[item]) target)

incrementTally :: Int -> Business n ()
incrementTally = tell . tick

-- Reader-Writer-State monad where
-- we can read a bunch of brains
-- we can modify some lists of worry
-- we can output counts for each monkey
type Business n a = RWS [Brain] Tally (Map Int [n]) a

newtype Tally = Tally (Map Int Int)
  deriving Show

instance Semigroup Tally where
  Tally t1 <> Tally t2 = Tally (M.unionWith (+) t1 t2)

instance Monoid Tally where
  mempty = Tally M.empty

tick :: Int -> Tally
tick i = Tally (M.singleton i 1)




-- Types which work as worry levels
class Worry a where
  add     :: Int -> a -> a
  times   :: Int -> a -> a
  square  :: a -> a
  divides :: Int -> a -> Bool
  relieve :: a -> a

instance Worry Int where
  add         = (+)
  times       = (*)
  square n    = n * n
  divides d n = n `mod` d == 0
  relieve n   = n `div` 3

instance Worry Ridiculous where
  add n     = onRidiculous (M.mapWithKey (\k r -> (r + n) `mod` k))
  times n   = onRidiculous (M.mapWithKey (\k r -> (r * n) `mod` k))
  square    = onRidiculous (M.mapWithKey (\k r -> (r * r) `mod` k))
  divides n = (== 0) . lookupRidiculous n
  relieve   = id -- no relief


-- Table of remainders mod k
newtype Ridiculous = Ridiculous (Map Int Int)
  deriving Show

onRidiculous f (Ridiculous table) = Ridiculous (f table)

toRidiculous :: [Int] -> Int -> Ridiculous
toRidiculous divisors n = Ridiculous table where
  table     = normalize (M.fromList (zip divisors (repeat n)))
  normalize = M.mapWithKey (\k r -> r `mod` k)

lookupRidiculous :: Int -> Ridiculous -> Int
lookupRidiculous k (Ridiculous table) = table ! k

-- Brains
data Brain = Brain Operation Test deriving Show

data Operation =
  OldPlus Int |
  OldTimes Int |
  OldSquared
    deriving Show

data Test = Test
  { testDivisor :: Int
  , testTrue    :: Int
  , testFalse   :: Int }
      deriving Show

applyOp :: Worry n => Operation -> n -> n
applyOp (OldPlus n)  = add n
applyOp (OldTimes n) = times n
applyOp OldSquared   = square

divisorOf (Brain _ (Test d _ _)) = d
