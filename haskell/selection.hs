import Data.List
ssort :: [Int] -> [Int]
ssort [] = []
ssort xs =   m : ssort (delete m xs)
               where m = minimum xs
