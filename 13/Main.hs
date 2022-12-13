module Main where

import Data.Char
import Data.Maybe
import Data.Ord
import Data.List

data T = I Int | L [T] deriving (Eq,Show,Read)
data K = C | Ɔ | N Int deriving (Eq,Ord,Show,Read)

tokenize :: String -> [K]
tokenize ('[':cs) = C : tokenize cs
tokenize (']':cs) = Ɔ : tokenize cs
tokenize (',':cs) = tokenize cs
tokenize "" = []
tokenize cs = N i : tokenize more where [(i,more)] = reads cs

parseTerms :: [K] -> ([T],[K])
parseTerms = go where
  go []         = ([], [])
  go (Ɔ:more)   = ([], more)
  go (N i:more) = (I i : xs, more') where (xs, more') = go more
  go (C:more)   = (L contents : xs, more'') where
    (contents,more' ) = go more
    (xs,      more'') = go more'

parseT :: String -> T
parseT = head . fst . parseTerms . tokenize

instance Ord T where
  I m  `compare` I n  = compare m n
  I m  `compare` y    = compare (L [I m]) y
  x    `compare` I n  = compare x (L [I n])
  L as `compare` L bs = go as bs where
    go [] [] = EQ
    go [] _  = LT
    go _  [] = GT
    go (a:as) (b:bs) = case compare a b of
      LT -> LT
      GT -> GT
      EQ -> go as bs

loadData :: FilePath -> IO [T]
loadData = fmap (map parseT . filter (/="") . lines) . readFile

pair (x1:x2:more) = (x1,x2) : pair more
pair []           = []

two = parseT "[[2]]"
six = parseT "[[6]]"

question1 :: [(T,T)] -> Int
question1 = sum . map fst . filter (uncurry (<) . snd) . zip [1..]

question2 :: [T] ->  Int
question2 = product . map fst . filter (g . snd) . zip [1..] . sort where
  g x = x==two || x==six

main = do
  foos <- loadData "input"
  print (question1 (pair foos))
  print (question2 (two:six:foos))
