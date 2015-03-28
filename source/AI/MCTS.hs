module AI.MCTS (
    -- data:
    Node,
    Tree,
    GameTree,
    -- data setup/usage:
    newNode,
    newTree,
    expandTree,
    -- main MCTS function:
    explore,
    -- operations on a tree:
    nMCTSRounds,
    bestMove,
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)
import Data.List (maximumBy)
import Game.Board
import System.Random (randomRIO)


-- data/types
--------------------------------------------------

data Node a = Node { visits :: Int
                   , score  :: Int
                   , board  :: a }

data Tree a = Tree a [Tree a] deriving (Eq)

type GameTree a = Tree (Node a)


-- constructing data
--------------------------------------------------

newNode :: (Board b) => b -> Node b
newNode b = Node { visits = 0
                 , score  = 0
                 , board  = b }

newTree :: (Board b) => b -> GameTree b
newTree b = Tree (newNode b) []

expandTree :: (Board b) => GameTree b -> GameTree b
expandTree (Tree n@(Node {board = b}) subtrees) = Tree n newTrees where
    newTrees = Tree <$> newNodes <*> []
    newNodes = map newNode $ choices b


-- main functions
--------------------------------------------------

explore :: (Board b) => GameTree b -> IO (Int, GameTree b)
explore tree@(Tree (Node visits score board) subtrees)
    | subtrees == [] = expansion
    | gameOver board = backpropagation
    | otherwise      = exploration
  where
    expansion = do
        expandedTree <- expandTree tree
        result <- simulate tree
        return (result, updateNodeWith result tree)
    backpropagation = do
        let result = scoreBoard board
        return (result, updateNodeWith result tree)
    exploration = do
        let nextTree = pickNode tree
        (result, newTree) <- explore nextTree
        return (negate result, updateNodeWith result tree)

simulate :: (Board b) => GameTree b -> IO Int
simulate tree = loop tree where
    loop (Tree Node {board = b} subtrees) = if gameOver b
        then return $ scoreBoard b
        else do
            nextTree <- randomElement subtrees
            loop nextTree


-- helper functions
--------------------------------------------------

ucb :: Int -> Node a -> Double
ucb t (Node {visits = v, score = s}) = (s' / v') + k * sqrt (log t' / v') where
    s' = fromIntegral s
    t' = fromIntegral t
    v' = fromIntegral v
    k  = sqrt 2.0
-- TODO: adjust k

pickNode :: (Board b) => GameTree b -> GameTree b
pickNode (Tree (Node {visits = v0}) ts) = maximumBy compareUCB ts where
    compareUCB (Tree n1 _) (Tree n2 _) = compare (ucb v0 n1) (ucb v0 n2)

updateNodeWith :: (Board b) => Int -> GameTree b -> GameTree b
updateNodeWith score (Tree node@(Node v0 s0 _) trees) = Tree newNode trees where
    newNode = node {visits = v0 + 1, score = s0 + score}

randomElement :: [a] -> IO a
randomElement xs = randomRIO (0, length xs - 1) >>= return . (xs !!)


-- functions for performing MCTS on a GameTree
--------------------------------------------------

mctsRound :: (Board b) => GameTree b -> IO (GameTree b)
mctsRound tree = explore tree >>= return . snd

nMCTSRounds :: (Board b) => Int -> GameTree b -> IO (GameTree b)
nMCTSRounds n tree = loopFor n tree where
    loopFor n tree = if n > 0
                         then mctsRound tree >>= loopFor (n - 1)
                         else return tree

bestMove :: (Board b) => GameTree b -> GameTree b
bestMove (Tree _ subtrees) = maximumBy compareTree subtrees where
    compareTree (Tree node1 _) (Tree node2 _) = compareNode node1 node2
    compareNode (Node {score = s1, visits = v1})
                (Node {score = s2, visits = v2})
                = compare (s1' / v1') (s2' / v2') where
                  s1' = fromIntegral s1 :: Double
                  v1' = fromIntegral v1 :: Double
                  s2' = fromIntegral s2 :: Double
                  v2' = fromIntegral v2 :: Double
