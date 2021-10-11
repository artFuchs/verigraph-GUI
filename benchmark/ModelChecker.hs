module Main where

import Test.Tasty.Bench
import System.Random
import Control.Monad

import qualified  Logic.Ctl                         as Logic
import qualified  Logic.Model                       as Logic


main :: IO ()
main = do
  expr <- return ( Logic.Temporal (Logic.A (Logic.F (Logic.Atom "a"))))
  [m1,m2,m3,m4,m5,m6] <- mapM notRandomModel [1,10,100,1000,10000,100000]
  defaultMain
    [ bgroup "SAT (AF a)"
      [ bench "1 state" $ whnf (Logic.satisfyExpr m1) expr
      , bench "10 states" $ whnf (Logic.satisfyExpr m2) expr
      , bench "100 states" $ whnf (Logic.satisfyExpr m3) expr
      , bench "1000 states" $ whnf (Logic.satisfyExpr m4) expr
      , bench "10000 states" $ whnf (Logic.satisfyExpr m5) expr
      --, bench "100000 states" $ whnf (Logic.satisfyExpr m6) expr
      ]
    ]
  return ()







-- create a model with a given number of states.
-- The model will have a tree like structure.
-- Random states will have the proposition "a"
randomModel :: Int -> IO (Logic.KripkeStructure String)
randomModel statesNum = do
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
notRandomModel :: Int -> IO (Logic.KripkeStructure String)
notRandomModel statesNum = do
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
