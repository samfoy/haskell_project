import System.IO

main :: IO ()
main = do
    a <- foldl (++) map (getLine) [0..]
    putStr ( a ++ "\n" )
    -- print IO getLine
    -- return ()