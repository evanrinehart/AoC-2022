module Main where

import Data.List
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IM
import Control.Monad.State

data Monkey n = Monkey
  { activity  :: Int
  , operation :: Operation
  , divisor   :: Int
  , target1   :: Int
  , target2   :: Int
  , inventory :: [n] }
      deriving Show

data Operation = OldPlus Int | OldTimes Int | OldSquared
    deriving Show

class Worry n where
  add     :: Int -> n -> n
  times   :: Int -> n -> n
  square  :: n -> n
  divides :: Int -> n -> Bool
  relieve :: n -> n

answer :: Worry n => Int -> [Monkey n] -> Int
answer numRounds monkeys = out where
  out       = n1 * n2
  [n1,n2]   = crunch (execState theRounds monkeyMap)
  monkeyMap = IM.fromList (zip [0..] monkeys)
  crunch    = take 2 . reverse . sort . map activity . IM.elems
  theRounds = replicateM_ numRounds $ do
    keys <- gets IM.keys
    forM_ keys $ \i -> do
      m@Monkey{operation=op,divisor=d,target1=t1,target2=t2} <- gets (! i)
      forM_ (inventory m) $ \lvl -> do
        let lvl'   = relieve (applyOp op lvl)
        let target = if d `divides` lvl' then t1 else t2
        modify (throwTo target lvl')
        modify (tick i)
      modify (clearInv i)

main = do
  monkeys1 <- loadData "input"
  let monkeys2 = makeRidiculous monkeys1
  print (answer 20    monkeys1)
  print (answer 10000 monkeys2)

-- misc
applyOp :: Worry n => Operation -> n -> n
applyOp (OldPlus n)  = add n
applyOp (OldTimes n) = times n
applyOp OldSquared   = square

onActivity  f m = m { activity = f (activity m) }
onInventory f m = m { inventory = f (inventory m) }

tick i      = IM.adjust (onActivity (+1)) i
throwTo i n = IM.adjust (onInventory (++[n])) i
clearInv i  = IM.adjust (onInventory (const [])) i

-- Number tracked as table of remainders mod k
newtype Ridiculous = Ridiculous (IntMap Int)
  deriving Show

toRidiculous :: [Int] -> Int -> Ridiculous
toRidiculous moduli n = Ridiculous table where
  table     = normalize (IM.fromList (zip moduli (repeat n)))
  normalize = IM.mapWithKey (\k r -> r `mod` k)

lookupRidiculous :: Int -> Ridiculous -> Int
lookupRidiculous k (Ridiculous table) = table ! k

onRidiculous f (Ridiculous table) = Ridiculous (f table)

makeRidiculous :: [Monkey Int] -> [Monkey Ridiculous]
makeRidiculous ms = map f ms where
  f m      = m {inventory = map (toRidiculous divisors) (inventory m)}
  divisors = map divisor ms

-- Ridiculous works where a Worry is required
instance Worry Ridiculous where
  add n     = onRidiculous (IM.mapWithKey (\k r -> (r + n) `mod` k))
  times n   = onRidiculous (IM.mapWithKey (\k r -> (r * n) `mod` k))
  square    = onRidiculous (IM.mapWithKey (\k r -> (r * r) `mod` k))
  divides n = (== 0) . lookupRidiculous n
  relieve   = id -- no relief

instance Worry Int where
  add         = (+)
  times       = (*)
  square n    = n * n
  divides d n = n `mod` d == 0
  relieve n   = n `div` 3


-- loader
loadData :: FilePath -> IO [Monkey Int]
loadData = fmap (map parseMonkey . splitOn "" . lines) . readFile

parseMonkey :: [String] -> Monkey Int
parseMonkey [l1,l2,l3,l4,l5,l6] = Monkey 0 op d t1 t2 ns where
  gut c = tail . dropWhile (/= c)
  strip = dropWhile (== ' ')
  ns    = (map (read . strip) . splitOn ',' . gut ':') l2
  op    = (parseOp . tail . gut '=' . gut ':') l3
  d     = (read . tail . gut 'y') l4
  t1    = (read . tail . gut 'y') l5
  t2    = (read . tail . gut 'y') l6
  parseOp ('o':'l':'d':' ':'+':' ':str) = OldPlus (read str)
  parseOp "old * old"                   = OldSquared
  parseOp ('o':'l':'d':' ':'*':' ':str) = OldTimes (read str)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn z l@(x:xs)
  | x==z      = splitOn z xs
  | otherwise = let (h,t) = break (==z) l in h:(splitOn z t)
