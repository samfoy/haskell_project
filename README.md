haskell_project
===============
### Crime and Sorting

#Sorting
## What it does
For this first project assignment we had to display IO, Data Structure and Control Structures in our language.
We (Sean and Sam) have combined all into one program.

### Data Structure
Our Structure is a Binary Search Tree. The tree structure is defined as:

    data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Show, Eq, Read)

This defines a tree as either empty (Nil) or as a Node with a value and 2 associated Trees.
Calling it 'Tree a' opposed to 'Tree Int' or the like essentially means the tree is a polymorphic data type.
The functions not needed to 'sort a list' through use of the tree are omitted.

### Control Structures
The program displays a number of control structures in Haskell.
The insert function demonstrates how to apply a different function definition on a case by case basis:

    insert :: (Ord a) => Tree a -> a -> Tree a
    insert Nil x = singleton x
    insert (Node left v right) x
        | x == v = Node left v right
        | x < v = Node (insert left x) v right
        | x > v = Node left v (insert right x) 

Recursion is displayed in both the insert function and the inorder traversal function below:

    inorder :: (Ord a) => Tree a -> [a]
    inorder Nil = []
    inorder (Node left v right) = inorder left ++ [v] ++ inorder right
    
Function composition is also prominently displayed throughout the program. 

### Input/Output
Input and output is in the main function in the form of getArgs, readFile, and writeFile

    main = do
        args <- getArgs
        contents <- readFile (args !! 0)
        let list =  split (==',') (filter (/= '\n') contents)
        let intlist = map readInt list
        writeFile "output.txt" (stripChars "[]" (show (sort intlist)) ++ "\n")

## Compiling
Compile with:

    ghc -o sort sort.hs

## Running
Supply an input file as a command line Argument:

    input.txt
        -1,2,0,-78,45,3,etc
        

    ./sort input.txt
    
The output will be printed to "output.txt"

# Crime

## What it does
We decided for our common program to implement 3 is a crime in Haskell.

**3 is a crime** is a logic game for 2 or 3 players. There are 7 suspects in a case. Only 3 of them are guilty.
In each round 3 random suspects are displayed for the players. 0, 1, or 2 of these suspects is guilty.
In each round the players can guess who the three criminals are or they can pass.
If a player guesses wrong they lose, and if a player guesses right they win. 

## Points of note
Writing a game in Haskell turned out to be an interesting exercise since games, by their nature, rely on I/O, and I/O and haskell do not exactly go hand in hand.
Instead of having a game loop, like one might create in an imperitive programming language, our game uses a recursive getCommand function that passes a GameState data structure. The action to be taken in each call to the getCommand function is determined by a series of guards and case statements.

## Compiling
Compile with

    ghc -o crime crime.hs

## Running

    ./crime




