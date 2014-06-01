import System.IO
import qualified System.IO.Error
import Control.Exception

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty

pp :: Show a => Tree a -> IO ()
pp = (mapM_ putStrLn) . treeIndent
   where
        treeIndent Empty                = ["-- /-"]
        treeIndent (Node lb v rb) =
                   ["--" ++ (show v)] ++
                   map ("  |" ++) ls ++
                   ("  `" ++ r) : map ("   " ++) rs
                   where
                        (r:rs) = treeIndent $ rb
                        ls     = treeIndent $ lb
                        
insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty x = Node (Empty) x (Empty)
insert (Node left v right) x
       | x == v = Node left v right
       | x < v = Node (insert left x) v right
       | x > v = Node left v (insert right x)
       
inorder_traverse :: Tree String -> String
inorder_traverse Empty = ""
inorder_traverse (Node left v right) =
    (inorder_traverse left) ++ v ++ "\n" ++ (inorder_traverse right)



main :: IO ()
main = mainLoop Empty

mainLoop :: Tree String -> IO ()
mainLoop tr =
      do a <- try (getLine)
         case a of
           Left e ->
               if System.IO.Error.isEOFError e
                  then do
                      putStrLn $ inorder_traverse tr
                      return ()
                  else ioError e
           Right str ->
               mainLoop $ insert tr str
           
