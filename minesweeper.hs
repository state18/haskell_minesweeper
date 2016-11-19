import Control.Monad
import System.Random
import System.IO.Unsafe
import System.Console.ANSI
import Text.Printf
import Text.Read
import Data.List
import Data.List.Split
import Data.Char

{-----------------------------------------------------------------------------
    Board Parameters
------------------------------------------------------------------------------}
--boardWidth = 10
--boardHeight = 10
-- Representation of mines/flags on the displayed board.
bombString = "M"
coveredTileString = "X"
--tileNum = boardWidth * boardHeight
-- Higher means bombs occur less
--bombChance = 7

data BoardParameters = BoardParameters { width :: Int
                                       , height :: Int
                                       , bombChance :: Int
                                       }

{-----------------------------------------------------------------------------
    Main/Gameloop
------------------------------------------------------------------------------}


main :: IO ()
main = do
    -- Pass initial board state to the gameLoop
    putStrLn "Hello! Please enter a difficulty level from 1 to 3, with higher being harder..."
    diffInput <- getLine
    if validateDifficultyInput diffInput == 0
        then do 
                setSGR [SetColor Foreground Vivid Yellow]
                putStrLn "Invalid Input! Try again."
                setSGR [Reset]
                main 
        else do
                let boardParams = genDifficultyParameters $ validateDifficultyInput diffInput
                gameLoop (setupBoard boardParams)(initialCoveredMap boardParams) boardParams
                -- Stop after player wins or loses game.
                putStrLn "Press enter to exit..."
                userExit <- getLine
                putStrLn "See you next time!"

gameLoop :: [[String]] -> [[String]] -> BoardParameters -> IO ()
gameLoop adjMat coveredMat boardParams = do
    printBoard coveredMat boardParams
    putStrLn "Enter tile to uncover with format -> row,column "
    userInput <- getLine
    -- let parsedInput = splitOn "," userInput
    -- let rowInput = read $ head(parsedInput) :: Int
    -- let columnInput = read $ head(tail(parsedInput)) :: Int
    let validatedInput = validateUserInput userInput boardParams
    -- It's game over if the chosen tile is a mine!
    if validatedInput == []
        then do 
                setSGR [SetColor Foreground Vivid Yellow]
                putStrLn "Invalid Input! Try again."
                setSGR [Reset]
                gameLoop adjMat coveredMat boardParams
        else do 
                let tilesToUncover = nub $ getTilesToRemove adjMat [] (validatedInput !! 0 -1) (validatedInput !! 1 -1) boardParams       
                -- let uncoveredTiles = uncoverTile  adjMat coveredMat (validatedInput !! 0 -1) (validatedInput !! 1 -1)
                let uncoveredTiles = uncoverTiles adjMat coveredMat tilesToUncover
                -- It's game over if the chosen tile is a mine!
                if adjMat !! (validatedInput !! 0 -1) !! (validatedInput !! 1 -1) == bombString
                    then do printBoard adjMat boardParams
                            setSGR [SetColor Foreground Vivid Red]
                            putStrLn "BOOM!!! GAME OVER!"
                            setSGR [Reset]
                            return ()
                    else if checkForWin adjMat uncoveredTiles boardParams == True
                            then do printBoard adjMat boardParams
                                    setSGR [SetColor Foreground Vivid Green]
                                    putStrLn "Congratulations! You win!"
                                    setSGR [Reset]                                   
                                    return ()                
                            else do    
                                gameLoop adjMat uncoveredTiles boardParams
    
    


{-----------------------------------------------------------------------------
    Validating User Input
------------------------------------------------------------------------------}    
validateDifficultyInput :: String -> Int
validateDifficultyInput input =
    if length input == 1 && (input == "1" || input == "2" || input == "3")
        then do read $ input :: Int
        else do
                0
                
validateUserInput :: String -> BoardParameters -> [Int]
validateUserInput input boardParams = do
    let splitInput = splitOn "," input
    let rowInput = read $ head(splitInput) :: Int
    let columnInput = read $ splitInput !! 1 :: Int

    if isInfixOf "," input && length(splitInput) == 2 && isInteger(splitInput !! 0) && isInteger(splitInput !! 1) && rowInput > 0 && 
       columnInput > 0 && rowInput <= height boardParams && columnInput <= width boardParams
        then [rowInput,columnInput]
        else []

    
isInteger :: String -> Bool
isInteger text =
    length(text) > 0 && sum (map bool2int (map isDigit text)) == length(text)
{------------------------------------------------------------------------
    Setup/Initialization
-------------------------------------------------------------------------}

genDifficultyParameters :: Int -> BoardParameters
genDifficultyParameters diff =
    case diff of
        1 -> BoardParameters 8 8 5
        2 -> BoardParameters 15 15 5
        3 -> BoardParameters 30 30 7       


genBombs :: (RandomGen g) => g -> BoardParameters -> [Int]
genBombs gen boardParams =
    randomRs (1,(bombChance boardParams)) gen

-- printbombs=do
    -- randomGen <- newStdGen
    -- print $ take 50 $ genBombs randomGen
setupBoard :: BoardParameters -> [[String]]
setupBoard boardParams = do
    -- Get random numbers, indicating bombs
    let bombList = map filterBombs [mod x (bombChance boardParams) | x <- take ((width boardParams) * (height boardParams)) $ genBombs(unsafePerformIO(newStdGen)) boardParams];
    -- Turn list into list of lists (list of rows)
    let bombGrid = chunksOf (height boardParams) bombList
    --Determine adjacency matrix (how many bombs are around a given tile?)
    genAdjacency bombGrid boardParams
    
    -- Afterwards, we can place the GUI images of the uncovered tiles.
    
    -- Then, place the clickable covered tiles down that disappear when clicked.
    -- Perhaps flag each of the covered tiles somehow if they are on bomb spot.



    
-- -1 means there is a bomb
filterBombs :: Int -> Int
filterBombs b
    | b /= 0 = 0
    | otherwise = -1
    
genAdjacency :: [[Int]] -> BoardParameters -> [[String]]
genAdjacency bombGrid boardParams =
    -- Iterate through all tiles and check for adjacent bombs
    chunksOf (height boardParams) [getNeighborBombNum bombGrid x y boardParams | x <- [0..(height boardParams)-1], y <- [0..(width boardParams)-1]]

-- Given a grid and coordinate pair, returns # of neighboring bombs   
-- grid -> row -> column -> number of adjacent bombs 
getNeighborBombNum :: [[Int]] -> Int -> Int -> BoardParameters -> String
getNeighborBombNum bombList x y boardParams
    | bombList !! x !! y == -1 = bombString
    | otherwise = show (sum $ map bool2int [x /= 0 && y /= 0 && bombList !! (x-1) !! (y-1) == -1,
    x /= 0 && y /= (width boardParams)-1 && bombList !! (x-1) !! (y+1) == -1,
    x /= (height boardParams)-1 && y /= 0 && bombList !! (x+1) !! (y-1) == -1,
    x /= (height boardParams)-1 && y /= (width boardParams)-1 && bombList !! (x+1) !! (y+1) == -1,
    x /= 0 && bombList !! (x-1) !! y == -1,
    x /= (height boardParams)-1 && bombList !! (x+1) !! y == -1,
    y /= 0 && bombList !! x !! (y-1) == -1,
    y /= (width boardParams)-1 && bombList !! x !! (y+1) == -1])
        
        
        
        

--Iterate through each tile, skip if -1
-- Else check each adjacent square (remember to check if on edge)
-- Returns grid of 1's indicating covered tiles (size of game board)
initialCoveredMap :: BoardParameters -> [[String]]
initialCoveredMap boardParams =
    chunksOf (height boardParams) $ take (height boardParams * width boardParams) $ cycle [coveredTileString]  


{------------------------------------------------------------------------
    Game Events
-------------------------------------------------------------------------}

-- When user selects a tile, this script is called.
-- adjacency map -> binary covered tiles -> pickedX -> pickedY -> new binary covered
-- TODO Recursion will allow a DFS approach to uncover multiple tiles if user uncovers a 0.
uncoverTile :: [[String]] -> [[String]] -> Int -> Int -> [[String]] 
uncoverTile adjGrid covGrid x y = do
    let (yhead,_:ys) = splitAt y $ covGrid !! x
    let (xhead,_:xs) = splitAt x covGrid
    xhead ++ (yhead ++ (adjGrid !! x !! y) : ys) : xs

-- If the user's selected tile has 0 adjacent bombs, search around it and find others that should also
-- be uncovered.    This function will return the visited list of tiles to uncover.
getTilesToRemove :: [[String]] -> [[Int]]-> Int -> Int -> BoardParameters -> [[Int]]
getTilesToRemove adjGrid visited x y boardParams = do
    if x >= 0 && y >= 0 && x < (height boardParams) && y < (width boardParams) && adjGrid !! x !! y /= bombString && (elem [x,y] visited) == False
        then do 
                let newVisited = visited ++ [[x,y]]
                if adjGrid !! x !! y == "0"
                    then do
                            --[[x,y]] ++
                            --(getTilesToRemove adjGrid newVisited (x-1) y) ++
                            --(getTilesToRemove adjGrid newVisited (x+1) y) ++
                            --(getTilesToRemove adjGrid newVisited x (y-1)) ++
                            --(getTilesToRemove adjGrid newVisited x (y+1))
                            
                            let topNeighbor = getTilesToRemove adjGrid newVisited (x-1) y boardParams
                            let bottomNeighbor = getTilesToRemove adjGrid topNeighbor (x+1) y boardParams
                            let leftNeighbor = getTilesToRemove adjGrid bottomNeighbor x (y-1) boardParams
                            let rightNeighbor = getTilesToRemove adjGrid leftNeighbor x (y+1) boardParams
                            let topLeft = getTilesToRemove adjGrid rightNeighbor (x-1) (y-1) boardParams
                            let topRight = getTilesToRemove adjGrid topLeft (x-1) (y+1) boardParams
                            let bottomLeft = getTilesToRemove adjGrid topRight (x+1) (y-1) boardParams
                            [[x,y]] ++ (getTilesToRemove adjGrid bottomLeft (x+1) (y+1)) boardParams
                else newVisited
        else visited
-- Given a list of tiles to remove, uncover them all
uncoverTiles :: [[String]] -> [[String]] -> [[Int]] -> [[String]]
uncoverTiles adjGrid covGrid [] = covGrid
uncoverTiles adjGrid covGrid tiles = do
    -- nub $ getTilesToRemove adjGrid [] x y    
    uncoverTiles adjGrid (uncoverTile adjGrid covGrid ((head tiles) !! 0) ((head tiles) !! 1)) (tail tiles)
    

  
-- True if win, False if not
checkForWin :: [[String]] -> [[String]] -> BoardParameters -> Bool
checkForWin adjMat uncoveredMat boardParams = do
    -- Count # of covered tiles and # of bomb tiles.
    -- Player wins if (width * height) - numCovered == numBombs
    let numBombs = sum $ map bool2int [x == bombString | x <- concat adjMat]
    let numUnCovered = sum $ map bool2int [x /= coveredTileString | x <- concat uncoveredMat]
    (width boardParams) * (height boardParams) - numUnCovered == numBombs
    
    
    
    
----------------------------------------------------------
-- Formatting/Printing Functions
----------------------------------------------------------
    
-- Recursively constructs a well formatted string representing
-- a row of the MineSweeper grid
printBoardRow :: [String] -> String
printBoardRow [] =
    "|"
printBoardRow a =
    "|" ++ printf "%2s " (head a) ++ (printBoardRow (tail a))
    
-- Responsible for displaying board to the user between actions    
printBoard :: [[String]] -> BoardParameters -> IO ()
printBoard bombGrid boardParams = do
    printBoardTop boardParams
    putStrLn $ printBoardBody bombGrid boardParams ((height boardParams)-1)
    
printBoardTop :: BoardParameters -> IO ()
printBoardTop boardParams = do
    
    putStrLn $ "      " ++ printBoardRow (map show [1..(width boardParams)])
    setSGR [SetColor Foreground Vivid Blue]
    putStrLn $ take ((width boardParams)*4 +8)(cycle "-")
    setSGR [Reset]

-- formats row and column data into a nice package to be displayed to user.
printBoardBody :: [[String]] -> BoardParameters -> Int -> String
printBoardBody board boardParams (-1) =
    ""
printBoardBody board boardParams row = 
    printf "%2d    " ((height boardParams)-row) ++ printBoardRow (board !! ((height boardParams)-row-1)) ++ 
        "\n" ++ (take ((width boardParams)*4 +8)(cycle "-") ++ "\n") ++ printBoardBody board boardParams (row-1) 

----------------------------------------------------------
-- Other Utility Functions
----------------------------------------------------------

bool2int :: Bool -> Int
bool2int b
    | b == True = 1
    | otherwise = 0        
-- sgrExample = do
    -- setSGR [SetColor Foreground Vivid Red]
    -- setSGR [SetColor Background Vivid Blue]
    -- putStr "Red-On-Blue"
    -- setSGR [Reset]
    -- putStr "White-On-Black"

