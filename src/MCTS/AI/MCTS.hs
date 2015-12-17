module MCTS.AI.MCTS where

import Control.Monad

import qualified MCTS.GameState as Game
import qualified MCTS.IOGame    as IOGame

-----------------------------------------------------------------------------

-- score: (Wins, Visits)
data Node a = Node { score     :: (Int, Int)
                   , gameState :: a
                   , choices   :: [Node a]
                   , terminal  :: Bool
                   , turn      :: Game.Player
                   , winner    :: Maybe Game.Winner
                   } deriving (Show)

instance (Game.GameState a) => Game.GameState (Node a) where
    choices  = choices
    gameOver = terminal
    winner   = winner
    turn     = turn

instance (IOGame.IOGame g) => IOGame.IOGame (Node g) where
    startGame = newNode (IOGame.startGame :: g)
    aiTurn n node = undefined
    playerTurn node = liftM newNode $ gameState node >>= playerTurn
    playGame n node = undefined
    showGame = IOGame.showGame . gameState

-----------------------------------------------------------------------------

newNode :: (Game.GameState a) => a -> Node a
newNode a = Node { score     = (0,0)
                 , gameState = a
                 , choices   = map newNode $ Game.choices a
                 , terminal  = Game.gameOver a
                 , turn      = Game.turn a
                 , winner    = Game.winner a
                 }

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
    exploration = do
        let (c, cs) = popBestChild node
        (result, c') <- explore c
        return . (,) (negate result) $
            node {score = (ws + result, vs + 1), choices = c':cs}

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

popBestChild :: (GameState a) => Node a -> (Node a, [Node a])
popBestChild node
    | visits node > 0 = popMaximumBy compareUCB $ Game.choices node
    | otherwise       = error "popBestChild: unexplored node"
  where
    compareUCB node1 node2 = compare (ucb vs node1) (ucb vs node2)
    vs = visits node

popMaximumBy :: (a -> a -> Ordering) -> [a] -> (a, [a])
popMaximumBy cmp list@(x:xs)
    | null list     = error "popMaximumBy: empty list"
    | null xs       = (x, [])
    | cmp m x == LT = (x, m:xs')
    | otherwise     = (m, x:xs')
  where
    (m, xs') = popMaximumBy cmp xs

ucb :: Int -> Node a -> Double
ucb t node = (w' / v') + k * sqrt (log t' / v')
  where
    (w, v) = score node
    w' = fromIntegral w
    t' = fromIntegral t
    v' = fromIntegral v
    k  = sqrt 2.0

-----------------------------------------------------------------------------

nMCTSRounds :: (GameState a, MonadRandom m) => Int -> Node a -> m (Node a)
nMCTSRounds = loop
  where
    loop n node
        | n > 0     = mctsRound node >>= loop (pred n)
        | otherwise = node
    mctsRound = liftM snd . explore
