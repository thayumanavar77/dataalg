import Data.List

permutation :: (Eq a) => [a] -> [[a]]
permutation [] = [[]]
permutation xs = [x:ys | x<-xs, ys<-permutation $ delete x xs]
