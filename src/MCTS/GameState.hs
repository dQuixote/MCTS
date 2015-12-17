module MCTS.GameState where

data Player = Player1
            | Player2
            deriving (Eq, Show)

data Winner = Only Player
            | None
            | Both
            deriving (Eq, Show)

class GameState a where
    choices  :: a -> [a]
    gameOver :: a -> Bool
    winner   :: a -> Maybe Winner
    turn     :: a -> Player

opponent :: Player -> Player
opponent Player1 = Player2
opponent Player2 = Player1
