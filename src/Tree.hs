module Tree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ (preorder left) ++ (preorder right)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = b3
  where b1 = foldTree f b left
        b2 = f a b1
        b3 = foldTree f b2 right

-- myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- myUnfoldr f b = go $ f b
--   where go Nothing = []
--         go (Just (a, nextB)) = [a] ++ (go $ f nextB)

unf :: (Num a, Ord a) => a -> Maybe (a,a,a)
unf a
  | a > 10 || a < -10 = Nothing
  | otherwise = Just (a + 2, a, a + 1)

unfold :: (Ord b) => (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = go (f a) Leaf
  where go Nothing tree = tree
        go (Just (a1, b, a2)) Leaf = (Node (go (f a1) Leaf) b (go (f a2) Leaf))
        go (Just (a1, b, a2)) (Node left n right) = (Node (go (f a1) left) b (go (f a2) right))

treeb :: (Num a, Ord a) => a -> a -> Maybe (a, a, a)
treeb n a
  | a + 1 > n = Nothing
  | otherwise = Just (a+1, a, a+1)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (treeb n) 0
