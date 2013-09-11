{- Sorting - Merge Sort
   thayumk@gmail.com
-}
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort xs') (mergesort ys)
               where (xs',ys) = splitAt (length xs `div` 2) xs
                
merge [] xs = xs
merge ys [] = ys
merge (x:xs) (y:ys)  
  | x<=y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys 
