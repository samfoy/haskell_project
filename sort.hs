import System.IO

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

main :: IO ()
main = do
    a <- getLine
    putStr ( a ++ "\n" )
    main
