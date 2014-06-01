data (Ord a, Eq a) => Tree a = Nil | Node (Tree a) a (Tree a)
     deriving Show
