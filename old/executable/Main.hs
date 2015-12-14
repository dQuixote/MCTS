module Main where

import AI.MCTS
import Game.PlayGame
import Game.GamesLibrary.Go
import Game.GamesLibrary.TicTacToe

main :: IO ()
main = makeSelection
    putStrLn $ "Please select a game:\n"
            ++ "(1) TicTacToe\n"
            ++ "(2) Go\n"
    input <- getLine
    makeSelection input

makeSelection :: IO ()
makeSelection = do
    putStrLn $ "Please select a game:\n"
            ++ "(1) Go\n"
            ++ "(2) TicTacToe\n"
    input <- getLine
    case input of
        "1" -> playGo
        "2" -> playTicTacToe
        _   -> do { putStrLn "error: invalid selection"; makeSelection }

playGo :: IO ()
playGo = undefined

playTicTacToe :: IO ()
playTicTacToe = undefined
