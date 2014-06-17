import qualified Data.Map as Map
import Data.List 
import System.IO
import System.Environment
import System.Random
import Data.Array.IO
import Control.Monad
import Control.Monad.Zip

data GameState = GameState {
  players_left::[Int],
  current_player::Int,
  criminals::[[Char]],
  last_guess::[[Char]]
} deriving (Show,Read)

possible_criminals = ["Albert","Baron","Curtis","Delilah","Erin","Frank","Gavin"]

criminals_this_round :: IO [[Char]]
criminals_this_round = liftM2 take (return 3) (shuffle possible_criminals)

suspects :: Int -> [[Char]] -> IO [[Char]]
suspects num cl = liftM2 (++) sus innocents
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
checkGuess cs gs = sort cs == sort gs

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
  putStrLn "There are 7 suspicious suspects."
  putStrLn "3 of them are actually criminals."
  putStrLn "The first person to name all 3 wins."
  putStrLn "If you guess incorrectly you lose."
  putStrLn "How many players: (2 or 3)"
  player_count <- getPlayerNumbers
  putStrLn "Lets get started."
  players <- return $ take player_count [1..10] :: IO [Int]
  criminals <- criminals_this_round
  let gs = GameState players 1 criminals [[]]
  getCommand gs

getCommand :: GameState -> IO()
getCommand gs
  | gameOver gs = do
    putStrLn "Thank you for playing"
    return ()
  | otherwise = do
    return ()

gameOver :: GameState -> Bool
gameOver gs@(GameState pl p cs lg) = (pl == []) || (checkGuess cs lg)
