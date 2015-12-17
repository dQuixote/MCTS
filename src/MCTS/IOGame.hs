module MCTS.IOGame where

import MCTS.GameState

-- aiTurn takes number of MCTS iterations to perform per turn as Int argument
class (GameState g) => IOGame g where
    startGame  :: g
    aiTurn     :: Int -> g -> IO g
    playerTurn :: g -> IO g
    playGame   :: Int -> g -> IO ()
    showGame   :: g -> IO ()
