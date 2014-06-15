import System.IO
import System.Environment
import System.Random

main = do
    -- Intro
    putStrLn "Welcome to the game of 3 of a crime."
    putStrLn "1) There are 7 people: A B C D E F G."
    putStrLn "2) 3 are perpetrators."
    putStrLn "3) 3 are chosen for each lineup."
    putStrLn "4) The first person to name all 3 wins."
    putStrLn "5) You may pass, but if you err, you lose.\n"
    -- Players
    putStrLn "Enter how many players: "
    player_count <- readLn :: IO Int
    putStrLn "Let's get started."
    
    lineup_loop [1..player_count] 1
    -- <- getLine
    -- contents <- readFile (args !! 0)
    -- let list =  split (==',') (filter (/= '\n') contents)
    -- let intlist = map readInt list
    -- writeFile "output.txt" (stripChars "[]" (show (sort intlist)) ++ "\n")

-- Takes the list of remaining players
lineup_loop :: [Int] -> Int -> IO ()
lineup_loop remaining_players lineup_round player_number = do
    putStrLn $ "Lineup " ++ (show lineup_round) ++ " =>   A B C   | 3 are guilty."
    next_remaining_players = filterM player_loop remaining_players
    putStrLn "Next round."
    
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM f lst = return lst
    
--player_loop :: Int -> IO Bool
player_loop player_number = do
    putStrLn $ "Player " ++ (show player_number) ++ "'s guess (enter to pass): "
    putStrLn "You win!"
    putStrLn "Better luck next time."
    putStrLn "Next player."
    x <- getLine
    return $ (x !! 0) == ("M" !! 0)