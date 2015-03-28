module Game.GamesLibrary.TicTacToe where

import Data.List as List
import Game.Board


-- data setup
--------------------------------------------------

type X = First
type O = Second

type Point = (Int,Int)
type Square = Maybe Player
type TTTBoard = [[Square]]

data TTTGame = Game TTTBoard Player deriving (Eq)

instance Board TTTGame where
    gameOver (Game board _) = isGameOver board
    choices game = map (move game) $ allowedMoves game
    scoreBoard = gameScore Second
    winner = getWinner
    turn (Game _ player) = player

showBoard :: TTTBoard -> String
showBoard board = header ++ mainBoard where
    header = "    "
          ++ (concatMap (\num -> show num ++ "   ") [0..length board - 1])
          ++ "\n"
    mainBoard = unlines . intersperse' horizontalRule . showRows $ board
    intersperse' x xs = [x] ++ intersperse x xs ++ [x]
    horizontalRule = "  " ++ (concat $ replicate (length board) "+---") ++ "+"
    showRows = zipWith (\num row -> show num ++ " " ++ row) [0..] . map showRow
    showRow = concat . intersperse' "|" . map showSquare
    showSquare = maybe "   " (\p -> " " ++ show' p ++ " ")
    show' p = if p == First then "X" else "O"

initialGame :: Int -> TTTGame
initialGame size = Game (initialBoard size) X

initialBoard :: Int -> TTTBoard
initialBoard size = replicate size $ replicate size Nothing

defaultBoard :: TTTBoard
defaultBoard = initialBoard defaultSize

defaultSize :: Int
defaultSize = 3


-- functions for MCTS
--------------------------------------------------

move :: TTTGame -> Point -> TTTGame
move (Game board player) (x,y) = Game newBoard (nextPlayer player) where
    newBoard = prevRows ++ [modifiedRow] ++ nextRows
    prevRows = take y board
    nextRows = drop (y + 1) board
    modifiedRows = take x (board !! y)
                ++ [Just player]
                ++ drop (x + 1) oldRow

allowedMoves :: TTTGame -> [Point]
allowedMoves (Game board _) = let size = length board
    in [(x,y) | x <- [0..size-1]
              , y <- [0..size-1]
              , getSquare board (x,y) == Nothing]

allowedMove :: TTTGame -> Point -> Bool
allowedMove = flip elem . allowedMoves


-- helper functions
--------------------------------------------------

getSquare :: TTTBoard -> Point -> Square
getSquare board (x,y) = (board !! x) !! y

chains :: TTTBoard -> [[Square]]
chains b = rows b ++ columns b ++ [diag1 b] ++ [diag2 b] where
    rows = id
    columns = transpose
    diag1 = diagonal
    diag2 = diagonal . map reverse
    diagonal = flip (zipWith (!!)) [0..]


-- game over, score functions
--------------------------------------------------

-- check if the game is over
isGameOver :: TTTGame -> Bool
isGameOver game = gameWon game || gameTie game

gameWon :: TTTGame -> Bool
gameWon (Game board player) = any full $ chains board where
    full = all (== Just player')
    player' = nextPlayer player

gameTie :: TTTGame -> Bool
gameTie = null . allowedMoves

getWinner :: TTTGame -> Maybe Player
getWinner game@(Game _ player) = if gameWon game
    then Just $ nextPlayer player
    else Nothing

gameScore :: Player -> TTTGame -> Int
gameScore player game
    | winner' == Nothing     = 0
    | winner' == Just player = 1
    | otherwise              = -1
    where winner' = getWinner game
