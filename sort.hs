import System.IO

main :: IO ()
main = do
    a <- getLine
    putStr ( a ++ "\n" )
    -- print IO getLine
    -- return ()