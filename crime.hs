
import Data.List 
import System.IO
import System.Random
import Data.Array.IO
import Control.Monad
import Data.Char
import Data.Maybe

data GameState = GameState {
  players_left::[Int], -- Array of players left in the competition
  current_player::Int, -- The current player, if 0 we will start a new round
  criminals::[[Char]], -- List of criminals for this game
  last_guess::[[Char]], -- The last guess, used to check if game won
  round_suspects::[[Char]] -- List of suspects for the current round
} deriving (Show,Read)

possible_criminals = ["Albert","Baron","Curtis","Delilah","Erin","Frank","Gavin"]

criminals_this_round :: IO [[Char]]
criminals_this_round = liftM2 take (return 3) (shuffle possible_criminals)

suspects :: Int -> [[Char]] -> IO [[Char]]
suspects num cl = do
  susp <- liftM2 (++) sus innocents
  shuffle susp
  where
    sus = liftM2 take (return num) (shuffle cl)
    innocents = liftM2 take (left) (shuffle others)
      where
        left = liftM2 (-) (return 3) (return num)
        others = filter (\x -> notElem x cl) possible_criminals

numberToShow :: IO Int
numberToShow = randomRIO (0,2) :: IO Int

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
             j <- randomRIO (i,n)
             vi <- readArray ar i
             vj <- readArray ar j
             writeArray ar j vi
             return vj
            where
                n = length xs
                newArray :: Int -> [a] -> IO (IOArray Int a)
                newArray n xs = newListArray (1,n) xs

checkGuess :: [[Char]] -> [[Char]] -> Bool
checkGuess cs gs = sort (map (map toLower) cs) == sort (map (map toLower) gs)

getPlayerNumbers :: IO Int
getPlayerNumbers = do
  players <- readLn :: IO Int
  if players == 2 || players == 3
    then return players
    else do
      putStrLn "Please enter 2 or 3."
      getPlayerNumbers

main :: IO ()
main = do
  putStrLn "Welcome to the game of 3 is a crime. A fun game for 2 or 3 people."
  putStrLn $ "There are 7 suspicious suspects, whose names are: " ++ (drop 2 $ foldl ( \x y -> x ++ ", " ++ y) "" possible_criminals)
  putStrLn "3 of them are actually criminals."
  putStrLn "The first person to name all 3 wins."
  putStrLn "If you guess incorrectly you lose."
  putStrLn "How many players: (2 or 3)"
  player_count <- getPlayerNumbers
  putStrLn "Lets get started."
  players <- return $ take player_count [1..10] :: IO [Int]
  criminals <- criminals_this_round
  let gs = GameState players 0 criminals [[]] [[]]
  getCommand gs

getCommand :: GameState -> IO()
getCommand gs@(GameState pl p cs lg ctr)
  | gameOver gs = do
      case (pl) of
        [] -> putStrLn ("You are all losers. ")
        _ -> putStrLn ("Congratulations to Player " ++ (show p) ++ ".")
      putStrLn "Thank you for playing."
      putStrLn "Play again? (yes or no)"
      input <- getLine
      let cmd = map toLower input
      case cmd of
        "yes" -> main 
        "no" -> return ()
      return ()
  | newRound gs = do
      case (length pl) of
        1 -> putStrLn "There is one player left this round."
        _ -> putStrLn ("There are " ++ (show $ length pl) ++ " players left this round.")
      numberOfCriminalsThisRound <- numberToShow
      suspectsThisRound <- suspects numberOfCriminalsThisRound (cs)
      case numberOfCriminalsThisRound of
        1 -> putStrLn ("Among " ++ (show suspectsThisRound) ++ " there is " ++ (show numberOfCriminalsThisRound) ++ " criminal.")
        _ -> putStrLn ("Among " ++ (show suspectsThisRound) ++ " there are " ++ (show numberOfCriminalsThisRound) ++ " criminals.") 
      getCommand (GameState pl (head pl) cs lg suspectsThisRound)
  | otherwise = do
      putStrLn ("Time for player " ++ (show p) ++ " to guess or pass (or quit).")
      input <- getLine
      let cmd = map toLower input
      case cmd of
        "pass" -> getCommand (GameState pl (nextPlayer gs) cs lg ctr)
        "quit" -> return ()
        "exit" -> return ()
        "guess" -> do
          putStrLn "Please enter the suspects' names (eg: \"Albert Delilah Erin\")"
          rawguess <- getLine
          let guess = words rawguess
          if checkGuess guess cs 
            then getCommand (GameState pl p cs guess ctr)
            else getCommand (GameState (filter (/= p) pl) (nextPlayer gs) cs guess ctr)
        _ -> do
          putStrLn "I do not know that command please enter guess or pass"
          getCommand gs

nextPlayer :: GameState -> Int
nextPlayer gs@(GameState pl p cs lg ctr)
  | p == last pl = 0
  | otherwise = pl!!x
    where x = fromJust(liftM2 (+) (return 1) (elemIndex p pl))

gameOver :: GameState -> Bool
gameOver gs@(GameState pl p cs lg ctr) = (pl == []) || (checkGuess cs lg)

newRound :: GameState -> Bool
newRound gs@(GameState pl p cs lg ctr) = (ctr == [[]]) || (p == 0)
