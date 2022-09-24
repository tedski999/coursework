module Main (main) where

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
    deriving Show

tree0 :: Tree Integer
tree0 = Leaf 10

tree1 :: Tree Integer
tree1 = Node (Node (Leaf 20) (Leaf 0)) (Leaf 10)

-- returns the number of nodes in the provided tree
count :: Tree a -> Integer
count (Leaf _) = 1
count (Node l r) = 1 + (count l) + (count r)

-- reports the maximum depth of the tree
depth :: Tree a -> Integer
depth (Leaf _) = 1
depth (Node l r) = 1 + max (depth l) (depth r)

-- converts the tree to a list
flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l r) = flatten l ++ flatten r
-- flatten (Node (Leaf l) (Leaf r)) = [x, r]
-- flatten (Node (Node ll lr) (Leaf r)) = [r] ++ flatten ll ++ flatten lr
-- flatten (Node (Leaf l) (Node rl rr)) = [l] ++ flatten rl ++ flatten rr

main :: IO ()
main = do
    print $ count tree0
    print $ depth tree0
    print $ flatten tree0
    print $ count tree1
    print $ depth tree1
    print $ flatten tree1
