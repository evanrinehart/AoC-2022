module Main where

import Data.Tuple

data SA = SA Int Int deriving Show

parseSA :: String -> (SA, String)
parseSA str =
  let [(n1, '-':more)] = reads str 
      [(n2, rest)]     = reads more
  in (SA n1 n2, rest)

parseAP :: String -> (SA, SA)
parseAP str =
  let (sa1, ',':more) = parseSA str
      (sa2, _)        = parseSA more
  in (sa1, sa2)

loadData :: FilePath -> IO [(SA,SA)]
loadData = fmap (map parseAP . lines) . readFile

contains (SA a b) (SA c d) = a <= c && d <= b
apart    (SA a b) (SA c d) = b < c || d < a
overlaps x y               = not (apart x y)

main = do
  pairs <- loadData "input"
  let f = uncurry contains
  let g = uncurry overlaps
  print (length (filter (\p -> f p || (f . swap) p) pairs))
  print (length (filter g pairs))
