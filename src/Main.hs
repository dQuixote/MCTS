module Main (main) where

import System.Environment

import MCTS.Games
import MCTS.GameState
import MCTS.IOGame

gamesList :: (IOGame g) => [(g, String)]
gamesList = [ (startGame :: TTT, "TicTacToe")
            ]

games :: (IOGame g) => [g]
games = map fst gamesList

gameNames :: [String]
gameNames = map snd gamesList

printGames :: String
printGames = zipWith format [1..] gameNames
  where
    format n name = "(" ++ show n ++ ") " ++ name

main :: IO ()
main = do
    aiIterations <- getAiIterations
    getSelection >>= playGame aiIterations . (games !!) . pred
    return
  where
    getAiIterations = do
        input <- fmap head getArgs
        e <- try (readIO input) :: IO (Either IOError Int)
        case e of
            Left  _ -> error "need integer argument"
            Right r -> return r
    getSelection = do
        putStr $ "Select a game.\n" ++ printGames ++ "\n"
        input <- getLine
        e <- try (readIO input) :: IO (Either IOError Int)
        case e of
            Left  _ -> error "need integer argument"
            Right r
                | r <= nGames -> return r
                | otherwise   -> error "invalid selection"
    nGames = length gamesList
