module MCTS where

import Control.Monad

import qualified Game

-----------------------------------------------------------------------------

-- score: (Wins, Visits)
data Node a = Node { score    :: (Int, Int)
                   , choices  :: [Node a]
                   , terminal :: Bool
                   , turn     :: Game.Player
                   , winner   :: Maybe Game.Winner
                   } deriving (Show)

instance (Game.GameState a) => Game.GameState (Node a) where
    choices = choices
    gameOver = terminal
    winner = winner
    turn = turn

newNode :: (Game.GameState a) => Node a
newNode a = Node { score    = (0,0)
                 , choices  = map newNode $ Game.choices a
                 , terminal = Game.gameOver a
                 , turn     = Game.turn a
                 , winner   = Game.winner a }

wins :: Node a -> Int
wins = fst . score

visits :: Node a -> Int
visits = snd . score

-----------------------------------------------------------------------------

explore :: (GameState a, MonadRandom m) => Node a -> m (Int, Node a)
explore node -- @(Node score choices terminal turn winner)
    | vs == 0       = expansion
    | terminal node = backpropagation
    | otherwise     = exploration
  where
    (ws, vs) = score node
    expansion = do
        result <- simulate node
        return (result, node {score = (result, 1)})
    backpropagation = do
        let result = scoreNode a
        return (result, node {score = (ws + result, vs + 1)})
    exploration = undefined

simulate :: (GameState a, MonadRandom m) => Node a -> m Int
simulate = loop where
    loop a
        | terminal a = return $ scoreNode a
        | otherwise  = liftM (negate . simulate) . randomElement $ choices a
    randomElement xs = liftM (xs !!) $ randomRIO (0, pred $ length xs)

scoreNode :: (GameState a) => Node a -> Node a
scoreNode a
    | Game.turn a == Game.Player1 = result
    | otherwise                   = negate result
  where
    result = case Game.winner a of
        Only Player1 ->  1
        Only Player2 -> -1
        None         ->  0
        Both         ->  0

ucb :: Int -> Node a -> Double
ucb t (Node {visits = v, score = s}) = (s' / v') + k * sqrt (log t' / v')
  where
    s' = fromIntegral s
    t' = fromIntegral t
    v' = fromIntegral v
    k  = sqrt 2.0

pickNode :: (Board b) => GameTree b -> GameTree b
pickNode (Tree (Node {visits = v0}) ts) = maximumBy compareUCB ts where
    compareUCB (Tree n1 _) (Tree n2 _) = compare (ucb v0 n1) (ucb v0 n2)
