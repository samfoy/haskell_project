import System.IO

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

leaf :: a -> Tree a
leaf x = Node x Empty Empty

pp :: Show a => Tree a -> IO ()
pp = (mapM_ putStrLn) . treeIndent
   where
        treeIndent Empty                = ["-- /-"]
        treeIndent (Node v lb rb) =
                   ["--" ++ (show v)] ++
                   map ("  |" ++) ls ++
                   ("  `" ++ r) : map ("   " ++) rs
                   where
                        (r:rs) = treeIndent $ rb
                        ls     = treeIndent $ lb

main :: IO ()
main = do
    a <- getLine
    putStr ( a ++ "\n" )
    main
