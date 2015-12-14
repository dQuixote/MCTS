module AI.Gameplay (
    nMCTSRounds,
    bestMove
    ) where

import AI.MCTS

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

-- TODO:
-- functions for updating gametree according to human IO moves
