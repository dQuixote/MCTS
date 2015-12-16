module MCTS.Games.TicTacToe where

import Data.Maybe

import MCTS.GameState
import MCTS.IOGame

type X = Player1
type O = Player2

type Square = Maybe Player

data TTT = TTT { board  :: [[Square]]
               , player :: Player
               } deriving (Eq)

instance GameState TTT where
    choices  = choices
    gameOver = gameOver
    winner   = winner
    turn     = player

instance IOGame TTT where
    startGame  = defaultInitGame
    -- TODO
    aiTurn     = undefined
    playerTurn = undefined
    playGame   = undefined
    showGame   = undefined

-----------------------------------------------------------------------------

initGame :: Int -> TTT
initGame n = TTT (initBoard n) X

initBoard :: Int -> [[Square]]
initBoard n = replicate n $ replicate n Nothing

defaultInitGame :: TTT
defaultInitGame = initGame 3

-----------------------------------------------------------------------------

move :: TTT -> (Int, Int) -> TTT
move (TTT board player) (x, y) = TTT newBoard (opponent player)
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

gameOver :: TTT -> Bool
gameOver ttt = gameWon ttt || null (allowedMoves ttt)

gameWon :: TTT -> Bool
gameWon = any same . filter full . chains . board
  where
    full = all isJust
    same xs = all (== head xs) $ tail xs

gameTie :: TTT -> Bool
gameTie ttt = gameOver ttt && not (gameWon ttt)

winner :: TTT -> Winner
winner ttt@(TTT _ player)
    | gameWon ttt = opponent player
    | otherwise   = None
