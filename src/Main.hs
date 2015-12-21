module Main (main) where

import System.Environment

import MCTS.Games
import MCTS.GameState
import MCTS.IOGame

gamesList :: (IOGame g) => [(g, String)]
gamesList = [ (startGame :: Node TTT, "TicTacToe")
            ]

games :: (IOGame g) => [g]
games = map fst gamesList

gameNames :: [String]
gameNames = map snd gamesList

printGames :: String
printGames = zipWith format [1..] gameNames
  where
    format n name = "(" ++ show n ++ ") " ++ name

playGame :: (IOGame g) => Int -> g -> IO ()
playGame n game = setupGame n game >>= mainGame n
  where
    setupGame :: (IOGame g) => Int -> g -> IO g
    setupGame n game = do
        humanFirst <- randomIO :: IO Bool
        if humanFirst
            then do
                putStrLn "Human plays first."
                return game
            else do
                putStrLn "AI plays first."
                showGame game
                aiTurn n game
    -- in mainGame, human plays first
    mainGame :: (IOGame g) => Int -> g -> IO ()
    mainGame n game = do
        when (gameOver game) return
        game' <- playerTurn game
        game'' <- aiTurn n game
        mainGame n game''

main :: IO ()
main = do
    aiIterations <- getAiIterations
    selection <- liftM ((games !!) . pred) getSelection
    playGame aiIterations selection
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
