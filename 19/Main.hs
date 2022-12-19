module Main where

data Ingredient = Ore Int | Clay Int | Obsidian Int deriving (Eq,Ord,Show)
data Blueprint = BP 
  { bpID       :: Int
  , bpOreBot   :: [Ingredient]
  , bpClayBot  :: [Ingredient]
  , bpObsBot   :: [Ingredient]
  , bpGeodeBot :: [Ingredient] }
      deriving Show

gut c = drop 1 . dropWhile (/= c)
strip = dropWhile (== ' ')

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn z l@(x:xs)
  | x==z      = splitOn z xs
  | otherwise = let (h,t) = break (==z) l in h:(splitOn z t)

parseBlueprint str = BP i r1 r2 r3 r4 where
  [header,body] = splitOn ':' str
  i = read (gut ' ' header) :: Int
  recips = map (drop 4 . words) (splitOn '.' body)
  [r1,r2,r3,r4] = map parseCost recips

parseCost :: [String] -> [Ingredient]
parseCost []                 = []
parseCost (s1:s2:"and":more) = parseIngredient (read s1) s2 : parseCost more
parseCost (s1:s2:[])         = [parseIngredient (read s1) s2]

parseIngredient n "ore"      = Ore n
parseIngredient n "clay"     = Clay n
parseIngredient n "obsidian" = Obsidian n

loadData = fmap (map parseBlueprint . lines) . readFile

main = do
  bps <- loadData "input"
  mapM_ print bps
