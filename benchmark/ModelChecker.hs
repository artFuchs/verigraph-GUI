module Main where

import Test.Tasty.Bench
import System.Random
import Control.Monad

import qualified  Logic.Ctl                         as Logic
import qualified  Logic.Model                       as Logic


main :: IO ()
main = do
  let e0 = Logic.Atom "a"
  [e1,e2,e3,e4,e5,e6] <- return $ map nAF_a [1,10,100,1000,10000,100000]
  [m1,m2,m3,m4,m5,m6,m7,m8,m9] <- mapM binaryTree [1000,2000,3000,4000,5000,6000,7000,8000,9000]
  [r1,r2,r3,r4,r5,r6,r7,r8,r9] <- mapM randomTree [1000,2000,3000,4000,5000,6000,7000,8000,9000]
  [d1,d2,d3,d4,d5,d6,d7,d8,d9] <- mapM completeDigraph [100,200,300,400,500,600,700,800,900]
  defaultMain
    [ bgroup "SAT (AF a) - binary tree"
      [ bench "1000 states" $ whnf (Logic.satisfyExpr m1) e1
      , bench "2000 states" $ whnf (Logic.satisfyExpr m2) e1
      , bench "3000 states" $ whnf (Logic.satisfyExpr m3) e1
      , bench "4000 states" $ whnf (Logic.satisfyExpr m4) e1
      , bench "5000 states" $ whnf (Logic.satisfyExpr m5) e1
      , bench "6000 states" $ whnf (Logic.satisfyExpr m6) e1
      , bench "7000 states" $ whnf (Logic.satisfyExpr m7) e1
      , bench "8000 states" $ whnf (Logic.satisfyExpr m8) e1
      , bench "9000 states" $ whnf (Logic.satisfyExpr m9) e1
      ]
      -- [ bench "1 state" $ whnf (Logic.satisfyExpr m1) e1
      -- , bench "10 states" $ whnf (Logic.satisfyExpr m2) e1
      -- , bench "100 states" $ whnf (Logic.satisfyExpr m3) e1
      -- , bench "1000 states" $ whnf (Logic.satisfyExpr m4) e1
      -- , bench "10000 states" $ whnf (Logic.satisfyExpr m5) e1
      -- ]
    , bgroup "SAT (AF a) - random tree"
      [ bench "1000 states" $ whnf (Logic.satisfyExpr r1) e1
      , bench "2000 states" $ whnf (Logic.satisfyExpr r2) e1
      , bench "3000 states" $ whnf (Logic.satisfyExpr r3) e1
      , bench "4000 states" $ whnf (Logic.satisfyExpr r4) e1
      , bench "5000 states" $ whnf (Logic.satisfyExpr r5) e1
      , bench "6000 states" $ whnf (Logic.satisfyExpr r6) e1
      , bench "7000 states" $ whnf (Logic.satisfyExpr r7) e1
      , bench "8000 states" $ whnf (Logic.satisfyExpr r8) e1
      , bench "9000 states" $ whnf (Logic.satisfyExpr r9) e1
      ]
      -- [ bench "1 state" $ whnf (Logic.satisfyExpr r1) e1
      -- , bench "10 states" $ whnf (Logic.satisfyExpr r2) e1
      -- , bench "100 states" $ whnf (Logic.satisfyExpr r3) e1
      -- , bench "1000 states" $ whnf (Logic.satisfyExpr r4) e1
      -- , bench "10000 states" $ whnf (Logic.satisfyExpr r5) e1
      -- ]
    , bgroup "SAT (AF a) - complete digraph"
      [ bench "100 states" $ whnf (Logic.satisfyExpr d1) e1
      , bench "200 states" $ whnf (Logic.satisfyExpr d2) e1
      , bench "300 states" $ whnf (Logic.satisfyExpr d3) e1
      , bench "400 states" $ whnf (Logic.satisfyExpr d4) e1
      , bench "500 states" $ whnf (Logic.satisfyExpr d5) e1
      , bench "600 states" $ whnf (Logic.satisfyExpr d6) e1
      , bench "700 states" $ whnf (Logic.satisfyExpr d7) e1
      , bench "800 states" $ whnf (Logic.satisfyExpr d8) e1
      , bench "900 states" $ whnf (Logic.satisfyExpr d9) e1
      ]
      -- [ bench "1 state" $ whnf (Logic.satisfyExpr d1) e1
      -- , bench "10 states" $ whnf (Logic.satisfyExpr d2) e1
      -- , bench "100 states" $ whnf (Logic.satisfyExpr d3) e1
      -- , bench "1000 states" $ whnf (Logic.satisfyExpr d4) e1
      -- ]
    -- , bgroup "SAT (AFxN a) - 100 states - binary tree"
    --   [ bench "AFx1" $ whnf (Logic.satisfyExpr m3) e1
    --   , bench "AFx10" $ whnf (Logic.satisfyExpr m3) e2
    --   , bench "AFx100" $ whnf (Logic.satisfyExpr m3) e3
    --   , bench "AFx1000" $ whnf (Logic.satisfyExpr m3) e4
    --   , bench "AFx10000" $ whnf (Logic.satisfyExpr m3) e5
    --   , bench "AFx100000" $ whnf (Logic.satisfyExpr m3) e6
    --   ]
    -- , bgroup "SAT (AFxN a) - 100 states - random tree"
    --   [ bench "AFx1" $ whnf (Logic.satisfyExpr r3) e1
    --   , bench "AFx10" $ whnf (Logic.satisfyExpr r3) e2
    --   , bench "AFx100" $ whnf (Logic.satisfyExpr r3) e3
    --   , bench "AFx1000" $ whnf (Logic.satisfyExpr r3) e4
    --   , bench "AFx10000" $ whnf (Logic.satisfyExpr r3) e5
    --   , bench "AFx100000" $ whnf (Logic.satisfyExpr r3) e6
    --   ]
    -- , bgroup "SAT (AFxN a) - 100 states - complete digraph"
    --   [ bench "AFx1" $ whnf (Logic.satisfyExpr d3) e1
    --   , bench "AFx10" $ whnf (Logic.satisfyExpr d3) e2
    --   , bench "AFx100" $ whnf (Logic.satisfyExpr d3) e3
    --   , bench "AFx1000" $ whnf (Logic.satisfyExpr d3) e4
    --   , bench "AFx10000" $ whnf (Logic.satisfyExpr d3) e5
    --   --, bench "AFx100000" $ whnf (Logic.satisfyExpr d3) e6
    --   ]
    ]
  return ()


nAF_a :: Int -> Logic.Expr
nAF_a n = if n > 0
            then Logic.Temporal (Logic.A (Logic.F (nAF_a (n-1))))
            else Logic.Atom "a"


-- create a model with a given number of states.
-- The model will have a tree like structure.
-- Random states will have the proposition "a"
randomTree :: Int -> IO (Logic.KripkeStructure String)
randomTree statesNum = do
  states <- forM [1..statesNum] $ \i -> do
    value <- randomRIO (1,100 :: Int)
    hasProposition <- return $ if i == 0 then False else value > 95 -- 5% of chances of having the proposition "a"
    return $ Logic.State i (if hasProposition then ["a"] else [])
  levels <- setLevelsR statesNum []
  transitions <- if length levels > 1
    then do
      transitions <- forM [1..(length levels - 1)] $ \l -> do
        let possibleSrcs = levels !! (l-1)
            tgts = levels !! l
        forM tgts $ \tgt -> do
          choosenSrc <- randomRIO (0,length possibleSrcs)
          return $ Logic.Transition (tgt-1) choosenSrc tgt []
      return $ concat transitions
    else return []
  let model =  Logic.KripkeStructure states (filter (\t -> Logic.source t /= (-1)) transitions)
  return model

-- given a number of states n, divide the states from 1 to n in levels
-- each level have a random number of states
setLevelsR :: Int -> [[Int]] -> IO [[Int]]
setLevelsR 0 ls = return ls
setLevelsR n [] = setLevelsR (n-1) [[1]]
setLevelsR n ls = do
  nSts <- randomRIO (1,n)
  start <- return $ sum (concat ls)
  setLevelsR (n-nSts) ((take nSts [start..]):ls)



-- create a tree with a given number of states
-- each level will have MIN(rest of states, 2 * #states of previous level)
-- the leaf states will have the preposition "a"
-- this is probably best
binaryTree :: Int -> IO (Logic.KripkeStructure String)
binaryTree statesNum = do
  let levels = setLevels statesNum 0
  let fstWithProp = head (last levels)
  let states = map (\i -> Logic.State i (if i >= fstWithProp then ["a"] else [])) [1..statesNum]
  let transitions = map (\i -> Logic.Transition (i-1) (floor $ (fromIntegral i)/2) i []) [2..statesNum]
  let model = Logic.KripkeStructure states transitions
  return model

setLevels :: Int -> Int -> [[Int]]
setLevels 0 _ = []
setLevels n 0 = [1]:(setLevels (n-1) 1)
setLevels n lv = l:(setLevels n'' (lv+1))
  where
    leng = round $ 2**(fromIntegral lv)
    n' = n - leng
    n'' = if n' < 0 then 0 else n'
    l = take (min n leng) [leng..]


-- create a complete digraph with a given number of states
-- one random state will have the preposition "a"
completeDigraph :: Int -> IO (Logic.KripkeStructure String)
completeDigraph statesNum = do
  luckNum <- randomRIO (1,statesNum)
  let states = map (\i -> Logic.State i (if i==luckNum then ["a"] else [])) [1..statesNum]
  let pairs = concat $ map (\src -> map (\tgt -> (src,tgt)) $ deleteAt src [1..statesNum]) [1..statesNum]
  let transitions = zipWith (\i (src,tgt) -> Logic.Transition i src tgt []) [1..] pairs
  return $ Logic.KripkeStructure states transitions


deleteAt :: Int -> [a] -> [a]
deleteAt n l = l'
  where
    (a,b) = splitAt n l
    l' = case b of
          (c:cs) -> a ++ cs
          []     -> a
