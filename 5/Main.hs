module Main where

import Data.Maybe
import Data.List
import Data.Array

type Stack = [Char]
type Yard  = Array Int Stack

data Ins = Ins { insNum :: Int, insFrom :: Int, insTo :: Int } deriving Show

doIns1 :: Ins -> Yard -> Yard
doIns1 (Ins n from to) stacks = iterate (move from to) stacks !! n

doIns2 :: Ins -> Yard -> Yard
doIns2 (Ins n from to) = moveMany n from to

move :: Int -> Int -> Yard -> Yard
move from to stacks =
  let crate:rest = stacks ! from
      toStack    = stacks ! to
  in stacks // [(from, rest), (to, crate:toStack)]

moveMany :: Int -> Int -> Int -> Yard -> Yard
moveMany n from to stacks = 
  let (crates, rest) = splitAt n (stacks ! from)
      toStack        = stacks ! to
  in stacks // [(from, rest), (to, crates ++ toStack)]

readout :: Yard -> [Char]
readout = map head . elems

main = do
  (layout, insList) <- loadData "input"
  putStrLn $ readout (foldl' (flip doIns1) layout insList)
  putStrLn $ readout (foldl' (flip doIns2) layout insList)

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

parseLayout :: [String] -> Yard
parseLayout ls =
  let stacks = map catMaybes (transpose (map parseLine1 ls))
  in listArray (1, length stacks) stacks

parseIns :: String -> Ins
parseIns input =
  let ('m':'o':'v':'e':' ':str) = input
      [(n,' ':'f':'r':'o':'m':' ':rest)] = reads str
      [(from,' ':'t':'o':' ':rest')] = reads rest
      [(to,_)] = reads rest'
  in Ins n from to

loadData :: FilePath -> IO (Yard, [Ins])
loadData path = do
  ls <- fmap lines (readFile path)
  let [foo,bar] = splitOn (== "") ls
  return (parseLayout (init foo), map parseIns bar)
