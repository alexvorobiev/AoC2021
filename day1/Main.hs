import System.IO  
import Control.Monad

main = do

  handle <- openFile "data1.txt" ReadMode
  contents <- hGetContents handle
  
  let data1 = (map read $ words contents) :: [Int]
  
  print $ countJumps data1

  let data13 = zipWith3 (\x y z -> x + y + z) (drop 2 data1) (init . tail $ data1) (init . init $ data1)

  print $ countJumps data13

  hClose handle   

count :: (a -> Bool) -> [a] -> Int
count f xs =
  foldr (+) 0 $ map ((\x -> if x then 1 else 0) . f) xs

countJumps :: [Int] -> Int
countJumps xs =
  count (\p -> fst p < snd p) (zip ([10^6] ++ xs) (xs ++ [0]))