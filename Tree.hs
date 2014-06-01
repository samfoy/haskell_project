data Tree a = Nil | Node (Tree a) a (Tree a)
     deriving (Show, Eq)

insert :: (Ord a) => Tree a -> a -> Tree a
insert Nil x = Node (Nil) x (Nil)
insert (Node left v right) x
       | x == v = Node left v right
       | x < v = Node (insert left x) v right
       | x > v = Node left v (insert right x)
