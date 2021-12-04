main = do
  print $ zipWith3 (\x y z -> [x, y, z]) ([1, 2, 3]::[Int]) ([11, 22, 33]::[Int]) ([111, 222, 333]::[Int])