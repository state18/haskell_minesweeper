import Control.Monad
import Control.Monad
import System.Random
import System.IO.Unsafe
import System.Console.ANSI
import Paths
import Text.Printf
import Data.List.Split
import Data.Char
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}


main :: IO ()
main = do
    -- Pass initial board state to the gameLoop
    gameLoop setupBoard $ initialCoveredMap boardHeight boardWidth
    


gameLoop :: [[Int]] -> [[Int]] -> IO ()
gameLoop adjMat coveredMat = do
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
            putStrLn "TODO"
            printBoard uncoveredTiles
            -- TODO Check for win/loss conditions! Also fix uncoverTile function to be both recursive and to replace
            -- the tiles with the corresponding ones in adjMat rather than 0
            gameLoop adjMat uncoveredTiles
    
    
canvasSize = 400
boardWidth = 16
boardHeight = 16
tileNum = boardWidth * boardHeight
-- Higher means bombs occur less
bombChance = 4

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

