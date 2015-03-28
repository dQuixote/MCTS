module Game.Board where

data Player = First
            | Second
            deriving (Eq)

nextPlayer :: Player -> Player
nextPlayer p = if p == First then Second else First

class Board b where
    -- these first three functions are technically
    -- all that is necessary to use MCTS:
    gameOver   :: b -> Bool
    choices    :: b -> [b]
    scoreBoard :: b -> Int
    -- and these are accessory functions for gameplay:
    winner     :: b -> Maybe Player
    turn       :: b -> Player
