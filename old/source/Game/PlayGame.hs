module Game.PlayGame (
    playGame
    ) where

import AI.MCTS
import Game.Board


-- main functions
--------------------------------------------------

playGame :: (Board b) => GameTree b -> Int -> IO ()
playGame game aiIterations =
    let board = getBoard . getNode $ game
        player = turn board
    in do
        putStr $ showBoard board
        hFlush stdout
        case player of
            First -> do
                putStr "It is your turn. "
                newGameTree <- getMove game
                continueGame newGameTree aiIterations
            Second -> do
                putStrLn "It is the AI's turn."
                newGameTree <- aiTurn game aiIterations
                continueGame newGameTree aiIterations

aiTurn :: (Board b) => GameTree b -> Int -> IO (GameTree b)
aiTurn game iterations = do
    gen <- newStdGen
    updateGame <- doNMCTSRounds iterations game
    let aiMove = pickBestMove updateGame
    return aiMove

-- TODO:
-- function which times doNMCTSRounds for
-- small n, and then updates GameData {time}
-- according to how long it took to run

playerTurn :: (Board b) => GameTree b -> IO (GameTree b)
playerTurn = getMove


-- helper functions
--------------------------------------------------

getBoard :: (Board b) => GameTree b -> b
getBoard (Tree (Node {board = b}) _) = b

-- getMove requests a move from the player,
-- and checks to make sure it is
--   (a) entered in the correct format
--   (b) a valid move.
-- if the input fails on either count, getMove calls itself again.
getMove :: (Board b) => GameTree b -> IO GameTree b
getMove game = let board = getBoard game in do
    putStr "Enter a move: "
    hFlush stdout -- to make sure putStr goes through
    line <- getLine
    input <- try (readIO line) :: IO (Either IOError Move)
    case input of
        Left _ -> do
            putStrLn "Invalid input format. Enter an integer duple."
            getMove game
        Right r -> if allowedMove board r
                       then return . gameMove game . move board $ r
                       else do
                           putStrLn "Invalid move."
                           getMove game

gameMove :: (Board b) => GameTree b -> b -> GameTree b
gameMove game@(Tree _ subtrees) newBoard = undefined

startGame :: (Board b) => b -> GameTree b
startGame board = undefined

continueGame :: (Board b) => GameTree b -> Int -> IO ()
continueGame game aiIterations = let board = getBoard game in
    if isGameOver game
        then endGame game
        else playGame game aiIterations

endGame :: (Board b) => GameTree b -> IO ()
endGame (Tree (Node {board = b}) _) = case winner board of
    Just First -> putStrLn "You win!"
    Nothing -> putStrLn "Tie."
    Just Second -> putStrLn "The AI wins."
