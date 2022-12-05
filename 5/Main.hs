#!cabal
{- cabal:
build-depends: base, mtl
-}
module Main where

import Data.Maybe
import Data.List
import Control.Monad.State

type Stack = [Char]
data Ins =
  Ins {
    insNum  :: Int,
    insFrom :: Int,
    insTo   :: Int
  } deriving Show

doIns1 :: Ins -> State [Stack] ()
doIns1 (Ins n from to) = replicateM_ n (move from to)

doIns2 :: Ins -> State [Stack] ()
doIns2 (Ins n from to) = moveMany n from to

move :: Int -> Int -> State [Stack] ()
move from to = modify $ \stacks ->
  let crate = head (stacks !! (from-1))
  in (update (from-1) tail . update (to-1) (crate:)) stacks

moveMany :: Int -> Int -> Int -> State [Stack] ()
moveMany n from to = modify $ \stacks ->
  let (crates, rest) = splitAt n (stacks !! (from-1))
  in (update (from-1) (const rest) . update (to-1) (crates++)) stacks

main = do
  (layout, insList) <- loadData "input"
  print $ map head (execState (mapM_ doIns1 insList) layout)
  print $ map head (execState (mapM_ doIns2 insList) layout)

-- misc

update :: Int -> (a -> a) -> [a] -> [a]
update 0 f (x:xs) = f x : xs
update n f (x:xs) = x : update (n-1) f xs

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
  | f x = splitOn f xs
  | otherwise = let (h,t) = break f l in h:(splitOn f t)

-- parse all the things

parseCrate :: String -> Maybe (Maybe Char, String)
parseCrate (' ':' ':' ':rest) = Just (Nothing, rest)
parseCrate ('[': c :']':rest) = Just (Just c, rest)
parseCrate _                  = Nothing

parseLine1 :: String -> [Maybe Char]
parseLine1 str = case parseCrate str of
  Just (crate, ' ':rest) -> crate : parseLine1 rest
  Just (crate, _       ) -> [crate]
  Nothing                -> []

parseLayout :: [String] -> [Stack]
parseLayout ls = map catMaybes (transpose (map parseLine1 ls))

parseIns :: String -> Ins
parseIns input =
  let ('m':'o':'v':'e':' ':str) = input
      [(n,' ':'f':'r':'o':'m':' ':rest)] = reads str
      [(from,' ':'t':'o':' ':rest')] = reads rest
      [(to,_)] = reads rest'
  in Ins n from to

loadData :: FilePath -> IO ([Stack], [Ins])
loadData path = do
  ls <- fmap lines (readFile path)
  return ls
  let [foo,bar] = splitOn (== "") ls
  return (parseLayout (init foo), map parseIns bar)

