module Main where

import Data.Tuple

data SA = SA Int Int
  deriving Show

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

contains :: (SA, SA) -> Bool
contains (SA a b, SA c d) = c >= a && d <= b

apart :: (SA, SA) -> Bool
apart (SA a b, SA c d) = b < c || d < a

overlaps = not . apart

main = do
  pairs <- loadData "input"
  print (length (filter (\p -> contains p || contains (swap p)) pairs))
  print (length (filter overlaps pairs))
