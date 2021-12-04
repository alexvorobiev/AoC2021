import System.IO  
import Control.Monad
import Data.List

data Cmd = Forward Int | Down Int | Up Int deriving (Show)

instance Read Cmd where 
  readsPrec _ str = 
    let instr = words str
        cmd = head instr 
        val = read (last instr) :: Int 
    in
      case cmd of 
        "forward" -> [(Forward val, "")]
        "up"      -> [(Up val, "")]
        "down"    -> [(Down val, "")]
  
main = do

  handle <- openFile "day2/data.txt" ReadMode
  contents <- hGetContents handle
  
  let data2 = map read (lines contents) :: [Cmd]

  -- part 1
  let coord = foldl' (\(pos, depth) cmd -> case cmd of
                      Forward v -> (pos + v, depth)
                      Up v      -> (pos, depth - v)
                      Down v    -> (pos, depth + v))
                (0, 0)
                data2

  
  print $ fst coord * snd coord

  -- part 2
  let coord = foldl' (\(pos, depth, aim) cmd -> case cmd of
                      Forward v -> (pos + v, depth + aim * v, aim)
                      Up v      -> (pos, depth, aim - v)
                      Down v    -> (pos, depth, aim + v))
                (0, 0, 0)
                data2

  print $ (\(pos, depth, _) -> pos * depth) coord

  hClose handle
  