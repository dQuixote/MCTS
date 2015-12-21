module MCTS.Games.TicTacToe where

import Data.Maybe
import System.Random

import           MCTS.AI.MCTS
import qualified MCTS.GameState as Game
import qualified MCTS.IOGame    as IOGame

type X = Game.Player1
type O = Game.Player2

type Square = Maybe Game.Player

data TTT = TTT { board  :: [[Square]]
               , player :: Game.Player
               } deriving (Eq)

instance Game.GameState TTT where
    choices  = choices
    gameOver = gameOver
    winner   = winner
    turn     = player

instance IOGame.IOGame TTT where
    startGame  = defaultInitGame
    -- TODO
    aiTurn     = undefined
    playerTurn = undefined
    showGame   = undefined

-----------------------------------------------------------------------------
-- GameState
-----------------------------------------------------------------------------

-- choices

move :: TTT -> (Int, Int) -> TTT
move (TTT board player) (x, y) = TTT newBoard (Game.opponent player)
  where
    newBoard = prevRows ++ [modifiedRow] ++ nextRows
    prewRows = take y board
    nextRows = drop (succ y) board
    modifiedRow = take x oldRow ++ [Just player] ++ drop (succ x) oldRow
    oldRow = board !! y

allowedMoves :: TTT -> [(Int, Int)]
allowedMoves (TTT board _) = [ (x, y)
                             | x <- [0..m]
                             , y <- [0..m]
                             , unoccupied (x, y)
                             ]
  where
    m = pred $ length board
    unoccupied = isNothing . getSquare board
    getSquare board (x, y) = (board !! y) !! x

choices :: TTT -> [TTT]
choices ttt = map (move ttt) $ allowedMoves ttt

-----------------------------------------------------------------------------

-- gameOver

gameOver :: TTT -> Bool
gameOver ttt = gameWon ttt || null (allowedMoves ttt)

gameWon :: TTT -> Bool
gameWon = any same . filter full . chains . board
  where
    full = all isJust
    same xs = all (== head xs) $ tail xs

gameTie :: TTT -> Bool
gameTie ttt = gameOver ttt && not (gameWon ttt)

-----------------------------------------------------------------------------

-- winner

winner :: TTT -> Game.Winner
winner ttt@(TTT _ player)
    | gameWon ttt = opponent player
    | otherwise   = Game.None

-----------------------------------------------------------------------------
-- IOGame
-----------------------------------------------------------------------------

-- startGame

initGame :: Int -> TTT
initGame n = TTT (initBoard n) X

initBoard :: Int -> [[Square]]
initBoard n = replicate n $ replicate n Nothing

defaultInitGame :: TTT
defaultInitGame = initGame 3

-----------------------------------------------------------------------------

-- aiTurn

aiTurn :: Int -> TTT -> IO TTT
aiTurn n ttt@(TTT board player) = do
    -- TODO
    newBoard <- undefined
    return $ TTT newBoard $ opponent player

-----------------------------------------------------------------------------

-- playerTurn

playerTurn :: TTT -> IO TTT
playerTurn ttt@(TTT board player) = do
    -- TODO
    newBoard <- undefined
    return $ TTT newBoard $ opponent player

-----------------------------------------------------------------------------

-- showGame

showGame :: TTT -> IO ()
showGame (TTT board player) = putStr $ header ++ showBoard
  where
    header = "    "
           ++ concatMap (\num -> show num ++ "   ") [0..length board - 1]
           ++ "\n"
    showBoard = unlines . intersperse' horizontalRule . showRows $ board
    intersperse' x xs = [x] ++ intersperse x xs ++ [x]
    horizontalRule = "  " ++ concat $ replicate (length board) "+---" ++ "+"
    showRows = zipWith (\num row -> show num ++ " " ++ row) [0..] . map showRow
    showRow = concat . intersperse' "|" . map showSquare
    showSquare = maybe "   " (\p -> " " ++ showPlayer p ++ " ")
    showPlayer p
        | p == Player1 = "X"
        | otherwise    = "O"

-----------------------------------------------------------------------------
