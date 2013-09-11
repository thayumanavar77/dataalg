import Data.List
combination :: (Eq a) => [a] -> [[a]]
combination [] = [[]]
combination (x:xs) = map (x:) (combination xs) ++ combination xs 