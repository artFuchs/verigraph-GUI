module Main where

import Test.Tasty.Bench
import System.Random
import Control.Monad

import qualified  Logic.Ctl                         as Ctl
import qualified  Logic.Ltl                         as Ltl
import qualified  Logic.Model                       as Logic


main :: IO ()
main = do
  [m1,m2,m3,m4,m5,m6,m7,m8,m9] <- mapM binaryTree [1000,2000,3000,4000,5000,6000,7000,8000,9000]
  [r1,r2,r3,r4,r5,r6,r7,r8,r9] <- mapM randomTree [1000,2000,3000,4000,5000,6000,7000,8000,9000]
  [d1,d2,d3,d4,d5,d6,d7,d8,d9] <- mapM completeDigraph [100,200,300,400,500,600,700,800,900]

  let benchCtl = map (\n ->
                      bench ("AF*"++(show n)++" a")
                        $ whnf (Ctl.satisfyExpr m1) (nAF_a n)) $ take 8 [1..]

  let benchLtl = map (\n ->
                      bench ("F*"++(show n)++" a")
                        $ whnf (Ltl.satisfyExpr m1 [0]) (nF_a n)) $ take 8 [1..]
  defaultMain
    [ bgroup "SAT (AF a) - binary tree" benchCtl
    , bgroup "SAT (F a) - binary tree" benchLtl
    ]
  return ()


nAF_a :: Int -> Ctl.Expr
nAF_a n = if n > 0
            then Ctl.Temporal (Ctl.A (Ctl.F (nAF_a (n-1))))
            else Ctl.Atom "a"

nF_a :: Int -> Ltl.Expr
nF_a n = if n > 0
            then Ltl.Temporal (Ltl.F (nF_a (n-1)))
            else Ltl.Atom "a"


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
