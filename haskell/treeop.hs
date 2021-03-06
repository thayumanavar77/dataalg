{- Binary tree related Manipulations.
  thayumk@gmail.com
-}
data BinaryTree a = Empty
                    | Node a (BinaryTree a) (BinaryTree a)
                      deriving (Eq, Ord, Show)                               


listToTree :: (Ord a) => [a] -> BinaryTree a
listToTree [] = Empty
listToTree (x:xs) = Node x (listToTree (filter (<= x) xs))
                           (listToTree (filter (> x) xs))

treeHeight :: BinaryTree a -> Int
treeHeight Empty = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right) 

preorder :: BinaryTree a -> [a]
preorder Empty = []
preorder (Node a left right) = [a] ++ preorder left ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Empty = []
postorder (Node a left right) = postorder left ++ postorder right ++ [a]

inorder :: BinaryTree a -> [a]
inorder Empty = []
inorder (Node a left right) = inorder left ++ [a] ++ inorder right

levelorder :: BinaryTree a -> [a]
levelorder bintree = levelorder' [bintree]
                     where 
                       levelorder' [] = []
                       levelorder' (Empty : xs') = levelorder' xs'
                       levelorder' (Node a left right:xs') = a : levelorder' (xs' ++ [left,right])
                       
mirrorTree :: BinaryTree a -> BinaryTree a
mirrorTree Empty = Empty
mirrorTree (Node a left right) = Node a (mirrorTree right) (mirrorTree left)

insertBST :: (Ord a) => a-> BinaryTree a -> BinaryTree a
insertBST x Empty = Node x Empty Empty
insertBST x (Node d left right) 
          | x < d = Node d (insertBST x left) right 
          | x > d = Node d left (insertBST x right)
                    
existsInBST :: (Ord a) => a -> BinaryTree a -> Bool                    
existsInBST _ Empty = False
existsInBST x (Node d left right) 
            | x == d = True
            | x < d = existsInBST x left
            | x > d = existsInBST x right
                      
--isBST :: BinaryTree a -> Bool
--isBST Empty = true
--isBST Node a left right = 