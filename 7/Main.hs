module Main where

import Data.List
import Data.Ord
import qualified Data.Map as M; import Data.Map (Map, (!))

data Cmd = GoList String [Entry] | GoBack
  deriving Show

data Entry = FileEnt Int String | DirEnt String
  deriving Show

type FileSystem = Map [String] [Entry]

main = do
  fs <- fmap buildFilesystem (loadData "input")

  let dirPaths = M.keys fs
  let sizes    = map (sizeOfDir fs) dirPaths
  let smalls   = filter (<= 100000) sizes
  print ("part 1", sum smalls)

  let pathSizes = zip dirPaths sizes
  let sorteds   = sortBy (comparing snd) pathSizes
  print ("part 2", find (wouldWork fs . fst) sorteds)

sizeOfDir :: FileSystem -> [String] -> Int
sizeOfDir fs base = sum (map g (fs ! base)) where
  g (FileEnt n _) = n
  g (DirEnt name) = sizeOfDir fs (name:base)

wouldWork :: FileSystem -> [String] -> Bool
wouldWork fs path =
  let freed  = sizeOfDir fs path
      unused = 70000000 - sizeOfDir fs ["/"] + freed
  in unused >= 30000000

buildFilesystem :: [Cmd] -> FileSystem
buildFilesystem = snd . foldl' (flip visitCmd) ([], M.empty)

-- boring

loadData :: FilePath -> IO [Cmd]
loadData path = fmap (parseCommands . lines) (readFile path)

parseCommands :: [String] -> [Cmd]
parseCommands [] = []
parseCommands ("$ cd ..":more) = GoBack : parseCommands more
parseCommands (('$':' ':'c':'d':' ':name):_:more) =
  let (contents, rest) = parseContents more
  in GoList name contents : parseCommands rest

parseContents :: [String] -> ([Entry], [String])
parseContents []     = ([], [])
parseContents (x:xs) = case parseEntry x of
  Just ent -> let (ents, rest) = parseContents xs in (ent:ents, rest)
  Nothing  -> ([], (x:xs))

parseEntry :: String -> Maybe Entry
parseEntry ('d':'i':'r':' ':more) = Just (DirEnt more)
parseEntry input = case reads input of
  [(size, ' ':more)] -> Just (FileEnt size more)
  _                  -> Nothing

visitCmd :: Cmd -> ([String], FileSystem) -> ([String], FileSystem)
visitCmd GoBack (cwd, fs)                 = (tail cwd, fs)
visitCmd (GoList name contents) (cwd, fs) = (cwd', fs') where
  cwd' = name : cwd
  fs'  = M.insert cwd' contents fs
