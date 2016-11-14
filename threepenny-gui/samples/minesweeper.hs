import Control.Monad
import System.Random
import System.IO.Unsafe
import System.Console.ANSI
import Paths
import Text.Printf
import Data.List.Split
import Data.Char


-- TODO (11/14):
-- Change interface to  pass around adjacency matrix and uncovered matrix as nested list of strings instead of integers.
--  This will allow better representations of tiles such as bombs, flags, and covered tiles.
--  Current key: uncovered tiles that aren't bombs will be displayed as number of neighboring bombs,
--               covered tiles will either be blank spaces or X. (X seems like a 'dangerous' letter though, but color code will help)
--               bombs will be B, or maybe M for mine
-- Make a function that checks to see if the player has won (all tiles that aren't bombs are revealed)
--  Note: This win condition will change if flagging is allowed in format -> f row,column and will require all bombs to be flagged AND
--  all non-bomb tiles uncovered.
-- Allow the removal of multiple tiles if player selects a tile with 0 neighboring bombs (depth first search?)
--  My current thoughts of DFS are inefficient because of how the visited tiles will be tracked. If I could have some external structure that
--  could be modified, then it wouldn't be inefficient. Regardless, it should still work even if it's redundant in checking some visited tiles.
--  It should NOT infinitely loop, which is good.

-- Optional/Extras: Flagging system as explained above. Flags will be represented by an F character. It is not possible to flag an uncovered tile.
--  Also it is not possible to uncover a flagged tile. To toggle flagging, simply try to flag the same tile again.
-- Add colored console output. The easiest way to do this, especially if another group member does it, is to just make a new function that expects
-- a string (board to show). The function will be responsible for printing certain characters in different colors. (switch statement here?)

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}


main :: IO ()
main = do
    -- Pass initial board state to the gameLoop
    gameLoop setupBoard $ initialCoveredMap boardHeight boardWidth
    


gameLoop :: [[Int]] -> [[Int]] -> IO ()
gameLoop adjMat coveredMat = do
    printBoard coveredMat
    putStrLn "Enter tile to uncover with format -> row,column "
    userInput <- getLine
    let parsedInput = splitOn "," userInput
    let rowInput = read $ head(parsedInput) :: Int
    let columnInput = read $ head(tail(parsedInput)) :: Int
    -- TODO: Validate user input here. Then check if they are on a bomb. Game ends if they are,
    -- else, uncover the appropriate tile(s)
    -- It's game over if the chosen tile is a mine!
    if adjMat !! (rowInput-1) !! (columnInput-1) == (-1)
        then do printBoard adjMat
                setSGR [SetColor Foreground Vivid Red]
                putStrLn "BOOM!!! GAME OVER!"
                setSGR [Reset]
                return ()
        else do
            let uncoveredTiles = uncoverTile  adjMat coveredMat (rowInput-1) (columnInput-1)
            
            -- TODO Check for win/loss conditions! Also fix uncoverTile function to be both recursive and to replace
            -- the tiles with the corresponding ones in adjMat rather than 0
            gameLoop adjMat uncoveredTiles
    
    
canvasSize = 400
boardWidth = 16
boardHeight = 16
tileNum = boardWidth * boardHeight
-- Higher means bombs occur less
bombChance = 5

genBombs :: (RandomGen g) => g -> [Int]
genBombs gen =
    randomRs (1,bombChance) gen

-- printbombs=do
    -- randomGen <- newStdGen
    -- print $ take 50 $ genBombs randomGen
setupBoard :: [[Int]]
setupBoard = do
    -- Get random numbers, indicating bombs
    let bombList = map filterBombs [mod x bombChance | x <- take tileNum $ genBombs(unsafePerformIO(newStdGen))];
    -- Turn list into list of lists (list of rows)
    let bombGrid = chunksOf boardHeight bombList
    --Determine adjacency matrix (how many bombs are around a given tile?)
    genAdjacency bombGrid 
    
    -- Afterwards, we can place the GUI images of the uncovered tiles.
    
    -- Then, place the clickable covered tiles down that disappear when clicked.
    -- Perhaps flag each of the covered tiles somehow if they are on bomb spot.



    
-- -1 means there is a bomb
filterBombs :: Int -> Int
filterBombs b
    | b /= 0 = 0
    | otherwise = -1
    
genAdjacency :: [[Int]] -> [[Int]]
genAdjacency bombGrid = do
    -- Iterate through all tiles and check for adjacent bombs
    chunksOf boardHeight [getNeighborBombNum bombGrid x y | x <- [0..boardHeight-1], y <- [0..boardWidth-1]]

-- Given a grid and coordinate pair, returns # of neighboring bombs   
-- grid -> row -> column -> number of adjacent bombs 
getNeighborBombNum :: [[Int]] -> Int -> Int -> Int
getNeighborBombNum bombList x y
    | bombList !! x !! y == -1 = -1 
    | otherwise = sum $ map bool2int [x /= 0 && y /= 0 && bombList !! (x-1) !! (y-1) == -1,
    x /= 0 && y /= boardWidth-1 && bombList !! (x-1) !! (y+1) == -1,
    x /= boardHeight-1 && y /= 0 && bombList !! (x+1) !! (y-1) == -1,
    x /= boardHeight-1 && y /= boardWidth-1 && bombList !! (x+1) !! (y+1) == -1,
    x /= 0 && bombList !! (x-1) !! y == -1,
    x /= boardHeight-1 && bombList !! (x+1) !! y == -1,
    y /= 0 && bombList !! x !! (y-1) == -1,
    y /= boardWidth-1 && bombList !! x !! (y+1) == -1]
        
        
        
        

    --Iterate through each tile, skip if -1
    -- Else check each adjacent square (remember to check if on edge)
-- Returns grid of 1's indicating covered tiles (size of game board)
initialCoveredMap :: Int -> Int -> [[Int]]
initialCoveredMap height width =
    chunksOf height $ take (height * width) $ cycle [1]  

-- When user selects a tile, this script is called.
-- adjacency map -> binary covered tiles -> pickedX -> pickedY -> new binary covered
uncoverTile :: [[Int]] -> [[Int]] -> Int -> Int -> [[Int]] 
uncoverTile adjGrid covGrid x y = do
    let (yhead,_:ys) = splitAt y $ covGrid !! x
    let (xhead,_:xs) = splitAt x covGrid
    xhead ++ (yhead ++ (adjGrid !! x !! y) : ys) : xs
    


onGameOver :: IO ()
onGameOver = do
    putStrLn "GAME OVER!"
    
    
bool2int :: Bool -> Int
bool2int b
    | b == True = 1
    | otherwise = 0

-- Prints string "a" a number of times    
printNTimes :: String -> Int -> IO ()
printNTimes a 1 =
    putStr a
    
printNTimes a num = do
    putStr a
    printNTimes a (num-1)
    
----------------------------------------------------------
-- Formatting/Printing Functions
----------------------------------------------------------
    
-- Recursively constructs a well formatted string representing
-- a row of the MineSweeper grid
printBoardRow :: [Int] -> String
printBoardRow [] =
    "|"
printBoardRow a =
    "|" ++ printf "%2d " (head a) ++ (printBoardRow (tail a))
    
-- Responsible for displaying board to the user between actions    
printBoard :: [[Int]] -> IO ()
printBoard bombGrid = do
    printBoardTop boardWidth
    putStrLn $ printBoardBody bombGrid (boardHeight-1)
    
printBoardTop :: Int -> IO ()
printBoardTop width = do
    
    putStrLn $ "      " ++ printBoardRow [1..width]
    setSGR [SetColor Foreground Vivid Blue]
    putStrLn $ take (boardWidth*4 +8)(cycle "-")
    setSGR [Reset]

-- formats row and column data into a nice package to be displayed to user.
printBoardBody :: [[Int]] -> Int -> String
printBoardBody board (-1) =
    ""
printBoardBody board row = 
    printf "%2d    " (boardHeight-row) ++ printBoardRow (board !! (boardHeight-row-1)) ++ 
        "\n" ++ (take (boardWidth*4 +8)(cycle "-") ++ "\n") ++ printBoardBody board (row-1) 
    
-- sgrExample = do
    -- setSGR [SetColor Foreground Vivid Red]
    -- setSGR [SetColor Background Vivid Blue]
    -- putStr "Red-On-Blue"
    -- setSGR [Reset]
    -- putStr "White-On-Black"

