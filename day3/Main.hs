import System.IO  
import Control.Monad
import Data.List

main = do

  handle <- openFile "day3/data.txt" ReadMode
  contents <- hGetContents handle
  
  -- each element is a column
  let data3 = transpose $ lines contents
 
  -- eash element is [#0s, #1s]
  let pairs = map ((map length).group.sort) data3

  let bits = map (\x -> head x < last x) pairs
  
  let gamma = bitsToInt bits

  let eps = bitsToInt (map not bits)

  print $ gamma * eps

  -- part 2
  
  let data3_2 = transpose data3

  print (take 12 data3_2)
  
  let o2 = fst $ (iterate (\(acc, d) -> filterStep acc (<=) d) ("", data3_2)) !! (length $ head data3_2)
 
  print $ take 12 $ (iterate (\(acc, d) -> filterStep acc (<=) d) ("", data3_2))

  print o2
  print $ bitsToInt (map (\b ->  b == '1') o2)

  let co2 = fst $ (iterate (\(acc, d) -> filterStep acc (>) d) ("", data3_2)) !! (length $ head data3_2)
  print co2
  --print $ take 6 $ (iterate (\(acc, d) -> filterStep acc  d) ("", data3_2))
  
  print $ bitsToInt (map (\b -> b == '1') co2)

  print $ (bitsToInt (map (\b ->  b == '1') o2)) *  (bitsToInt (map (\b ->  b == '1') co2))
  

  hClose handle 

bitsToInt bits =
  foldl' (\x (i, b) -> x + (if b then 2^i else 0))
                      0 
                      (zip [0..] (reverse bits))

-- filter and drop
filterStep :: String -> (Int -> Int -> Bool) -> [String] -> (String, [String])
filterStep acc fn d =
  let groups = group . sort . head . transpose $ d
      pair = map length groups
      
      c = if (length pair) == 1 then
            if fn (read [head . head $ d]) 1 then '0' else '1'
          else
            -- head is #0s, last is #1s
            if (fn (head pair) (last pair)) then '1' else '0'
      filtered = filter (\x -> head x == c) d
  in
    if (length filtered) == 0 then
      (acc, [])
    else if (length filtered) == 1 then
      (acc ++ (head filtered), [])
    else
      (acc ++ [c], transpose . tail . transpose $ filtered)