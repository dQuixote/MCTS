module Game.IOGame where

import Game.Board

class IOGame g where
    startGame  :: (Board b) => b -> g
    aiTurn     :: g -> Int -> IO g
    playerTurn :: g -> IO g
    playGame   :: g -> Int -> IO ()
    showGame   :: g -> String
