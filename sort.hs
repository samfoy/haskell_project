import System.IO
import System.Environment

data Tree a = Nil | Node (Tree a) a (Tree a)
     deriving (Show, Eq, Read)

singleton :: a -> Tree a
singleton x = Node Nil x Nil

insert :: (Ord a) => Tree a -> a -> Tree a
insert Nil x = singleton x
insert (Node left v right) x
       | x == v = Node left v right
       | x < v = Node (insert left x) v right
       | x > v = Node left v (insert right x)

inorder :: (Ord a) => Tree a -> [a]
inorder Nil = []
inorder (Node left v right) = inorder left ++ [v] ++ inorder right

fromList :: (Ord a) => [a] -> Tree a
fromList xs = foldl insert Nil xs

sort :: (Ord a) => [a] -> [a]
sort xs = inorder (fromList xs)

main = do
     args <- getArgs
     contents <- readFile (args !! 0)
     let list =  split (==',') (filter (/= '\n') contents)
     let intlist = map readInt list
     writeFile "output.txt" (stripChars "[]" (show (sort intlist)) ++ "\n")

readInt :: String -> Int
readInt = read

stripChars :: String -> String -> String
stripChars = filter . flip notElem

split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
      "" -> []
      s' -> w : split p s''
         where (w, s'') = break p s'
