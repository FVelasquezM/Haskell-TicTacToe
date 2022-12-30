module Lib (
    initialState,
    playGame
) where

import System.Random
import Control.Monad.State.Lazy

data Square = X | O | Empty deriving ( Eq, Show )
type Board = [Square]
data Winner = XWins | OWins | Ongoing | Draw deriving (Eq, Show)
--Current board & current move
data GameState = GameState{board :: Board, move::Int, winner :: Winner} deriving (Show)

playGame :: GameState -> IO ()
playGame s = do
    putStrLn "Please insert a move"
    move <- getLine
    playerResult <- runStateT (playerMove (read move :: Int) X) s
    if fst playerResult /= "Game Ongoing" then 
        do
            putStrLn $ fst playerResult
            print $ snd playerResult 
        else
            do 
                computerMove <- liftIO (computerPosition (board $ snd playerResult))
                computerResult <- runStateT (playerMove computerMove O) (snd playerResult)
                putStrLn $ fst computerResult
                print $ snd computerResult
                unless (fst computerResult /= "Game Ongoing") (playGame $ snd computerResult)

playerMove :: Int -> Square -> StateT GameState IO String
playerMove position player = do 
    gameState <- get
    let boardAfterMove = placeMove (board gameState) player position
    let winner = checkWinner boardAfterMove $ winnerForPlayer player
    let gs = checkIfWinnerOrOngoing winner player boardAfterMove
    put GameState{board = boardAfterMove, move = move gameState + 1, winner=winner} 
    return gs


checkIfWinnerOrOngoing :: Winner -> Square -> Board -> String
checkIfWinnerOrOngoing winner player board 
    | winnerForPlayer player == winner = "Game Won by: " ++ show player
    | checkDraw board = "Game Drawn"
    | otherwise = "Game Ongoing"


winnerForPlayer :: Square -> Winner 
winnerForPlayer X = XWins
winnerForPlayer O = OWins

checkWinner :: Board -> Winner -> Winner
checkWinner board player
    | checkWin board = player
    | checkDraw board = Draw
    | otherwise = Ongoing
    
initialState :: GameState
initialState = GameState (map (const Empty) [0..8]) 0 Ongoing

computerPosition :: Board -> IO Int
computerPosition board = do
    num <- randNum
    if positionUsed board num then computerPosition board else return num
    
randNum :: IO Int
randNum = randomRIO(0,8) :: IO Int

positionUsed :: Board -> Int -> Bool
positionUsed board position = (board !! position) /= Empty

-- Params: Current Board, Square to place (X or O) position to place it in.
placeMove :: Board -> Square -> Int -> Board
placeMove board square position = let (x, _: ys) = (splitAt position board) in 
    x ++ square : ys

checkDraw :: Board -> Bool
checkDraw board = not (checkWin board) && checkBoardFull board

checkBoardFull :: Board -> Bool
checkBoardFull = notElem Empty 

checkWin :: Board -> Bool
checkWin board = checkWinHorizontal board || checkWinVertical board || checkWinDiagonal board

checkWinHorizontal :: Board -> Bool
checkWinHorizontal = checkWinGeneric boardToRows

checkWinVertical :: Board -> Bool
checkWinVertical = checkWinGeneric boardToColumns

checkWinDiagonal :: Board -> Bool
checkWinDiagonal = checkWinGeneric boardToDiagonals

checkWinGeneric :: (Board -> [[Square]]) -> Board -> Bool
checkWinGeneric partitionFunction = any checkAllXorO . partitionFunction

checkAllXorO :: Board -> Bool
checkAllXorO row = all (== X) row || all (== O) row 

--should convert [0, 1, 2, 3, 4, 5, 6, 7, 8] to [[0,3,6],[1,4,7], [2,5,8]]
boardToColumns :: Board -> [[Square]]
boardToColumns board = [[head board, board !! 3, board !! 6], [board !! 1, board !! 3, board !! 7, board !! 2, board !! 5, board !! 8]] 

--should convert [0, 1, 2, 3, 4, 5, 6, 7, 8] to [[0,1,2],[3,4,5], [6,7,8]]
boardToRows :: Board -> [[Square]]
boardToRows board = [take 3 board, take 3 $ drop 3 board, take 3 $ reverse board]

--should convert [0, 1, 2, 3, 4, 5, 6, 7, 8] to [[0,4,8], [2, 4, 6]]
boardToDiagonals :: Board -> [[Square]]
boardToDiagonals board= [[head board, board !! 4, board !! 8], [board !! 2, board !! 4, board !! 6]]