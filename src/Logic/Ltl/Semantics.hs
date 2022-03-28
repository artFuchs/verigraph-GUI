module Logic.Ltl.Semantics (
  satisfyExpr
, modelXautomaton
) where

import Logic.Ltl.Base
import Logic.Ltl.Parser
import Logic.Ltl.Automaton

import qualified Logic.Model as Logic

import Control.Monad
import Data.Maybe (fromMaybe, catMaybes)
import Data.List ((\\), nub, union, sort)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM




-- | Checks if there is a path that satisfies 'Not expr'.
-- If the returned result is [] then the expression passed holds for the system
-- else the path passed is a counter example for the expression
satisfyExpr :: Logic.KripkeStructure String -> [Int] -> Expr -> IO [Int]
satisfyExpr model initial expr = do
  putStrLn $ "automatan for " ++ (show expr')
  mapM_ print (Logic.states $ snd na)
  mapM_ print (Logic.transitions $ snd na)

  putStrLn "\n model x automaton"
  mapM_ print (Logic.states $ mXna)
  mapM_ print (Logic.transitions $ mXna)
  print mapping

  putStrLn ""

  path <- findSatisfyingPath mXna expr' initialSts
  let path' = catMaybes $ map (\i -> IM.lookup i mapping) path

  return path'
  where


    -- combine model and automaton
    (initialSts, mXna, mapping) = modelXautomaton (initial,model') na

    -- create expression automaton
    na = exprAutomaton expr'
    expr' = rewriteExpr (Not expr)

    -- rewrite model to add the information of the transitions to the source states
    -- and to add loops to the states that are dead ends.
    model' = model{Logic.states = states', Logic.transitions = transitions'}
    transitions' = Logic.transitions model ++ extraTransitions
    extraTransitions = zipWith (\i s -> Logic.Transition i s s [])
                           [(length (Logic.transitions model))-1 ..]
                           (filter shouldHaveLoop $ Logic.stateIds model)
    states' = map addTrasitionsAtomsToState (Logic.states model)

    addTrasitionsAtomsToState :: Logic.State String -> Logic.State String
    addTrasitionsAtomsToState (Logic.State i v) = Logic.State i v'
      where
        stTransitions = filter (\(Logic.Transition _ s _ _) -> s == i) (Logic.transitions model)
        transitionsAtoms = map (Logic.values) stTransitions
        v' = v `union` (concat transitionsAtoms)

    shouldHaveLoop :: Int -> Bool
    shouldHaveLoop i = null stTransitions
      where
        stTransitions = filter (\(Logic.Transition _ s _ _) -> s == i) (Logic.transitions model)





findSatisfyingPath :: Logic.KripkeStructure Expr -> Expr -> [Int] -> IO [Int]
findSatisfyingPath model expr initialSts = do
  mapM_ print paths
  return $ case paths of
    (p:_) -> p
    _ -> []
  where
    paths = filter (not . null) $ concat $ map findPaths initialSts
    findPaths s = findSatisfyingPaths model expr [s]


findSatisfyingPaths :: Logic.KripkeStructure Expr -> Expr -> [Int] -> [[Int]]
findSatisfyingPaths model (Literal False) path@(i:is) = []

findSatisfyingPaths model t@(Literal True) path@(i:is) =
  case nextStates'' of
    [] -> []
    ns -> concat $ map (\n -> if n `elem` path then [n:path] else findSatisfyingPaths model t (n:path)) ns
  where
    nextStates = Logic.nextStates model i
    nextStates' = filter (`elem` path) nextStates
    nextStates'' = if null nextStates' then nextStates else nextStates'

findSatisfyingPaths model e@(Atom a) path@(i:is) =
  if e `elem` exprs
    then findSatisfyingPaths model (Literal True) path
    else []
  where
    exprs = Logic.values $ Logic.getState i model


findSatisfyingPaths model ne@(Not e) path@(i:is) =
  if ne `elem` exprs || e `notElem` exprs
    then findSatisfyingPaths model (Literal True) path
    else []
  where
    exprs = Logic.values $ Logic.getState i model

findSatisfyingPaths model xe@(Temporal (X e)) path@(i:is) =
  nextPaths
  where
    nextStates = Logic.nextStates model i
    nextPaths = filter (not . null) $ concat $ map (\n -> findSatisfyingPaths model e (n:path)) nextStates

findSatisfyingPaths model e@(Temporal (U a b)) path@(i:is) =
  case (e `elem` exprs, b `elem` exprs) of
    (True, True) -> findSatisfyingPaths model b path
    (True, False) -> nextPaths
    _ -> []
  where
    exprs = Logic.values $ Logic.getState i model
    nextStates = filter (`notElem` path) $ Logic.nextStates model i
    nextPaths = filter (not . null) $ concat $ map (\n -> findSatisfyingPaths model e (n:path)) nextStates

findSatisfyingPaths model e@(And a b) path@(i:is) =
  case (e `elem` exprs, pathA, pathB) of
    (True, a:as, b:bs) -> findSatisfyingPaths model (Literal True) path
    _ -> []
  where
    exprs = Logic.values $ Logic.getState i model
    pathA = findSatisfyingPaths model a path
    pathB = findSatisfyingPaths model b path

findSatisfyingPaths model e@(Or a b) path@(i:is) =
  case (e `elem` exprs, pathA, pathB) of
    (True, a:as, _) -> pathA
    (True, _, b:bs) -> pathB
    _ -> []
  where
    exprs = Logic.values $ Logic.getState i model
    pathA = findSatisfyingPaths model a path
    pathB = findSatisfyingPaths model b path

findSatisfyingPaths model e@(Implies a b) path@(i:is) =
  case (e `elem` exprs, pathA) of
    (True, a:as) -> pathB
    (True, []) -> findSatisfyingPaths model (Literal True) path
    _ -> []
  where
    exprs = Logic.values $ Logic.getState i model
    pathA = findSatisfyingPaths model a path
    pathB = findSatisfyingPaths model b path

findSatisfyingPaths model e@(Equiv a b) path@(i:is) =
  case (e `elem` exprs, pathA, pathB) of
    (True, a:as, b:bs) -> findSatisfyingPaths model (Literal True) path
    (True, [], []) -> findSatisfyingPaths model (Literal True) path
    _ -> []
  where
    exprs = Logic.values $ Logic.getState i model
    pathA = findSatisfyingPaths model a path
    pathB = findSatisfyingPaths model b path






-- | Given a model and a automaton for a LTL expression, combine the two,
modelXautomaton :: ([Int],Logic.KripkeStructure String) -> ([Int],Logic.KripkeStructure Expr) -> ([Int], Logic.KripkeStructure Expr, IntMap Int)
modelXautomaton (initialSts,model) (initialSts',autom) =
  (initialSts'', Logic.KripkeStructure states transitions, statesMapping)
  where
    statesMapping = IM.map fst statesPairs
    initialSts'' = IM.keys $ IM.filter (\(s,k) -> s `elem` initialSts && k `elem` initialSts') statesPairs
    transitions = joinTransitions model autom statesPairs
    states = IM.elems $ IM.mapWithKey (\i (s,k) -> Logic.State i (Logic.values . Logic.getState k $ autom)) statesPairs
    statesPairs = findCompatibleStates (Logic.states model) (Logic.states autom)


joinTransitions :: Logic.KripkeStructure String -> Logic.KripkeStructure Expr -> IntMap (Int, Int) -> [Logic.Transition Expr]
joinTransitions model autom statesPairs =
  zipWith (\i (s,t) -> Logic.Transition i s t []) [0..] atomstransitions
  where
    atomstransitions = concat $ IM.elems $ IM.mapWithKey (\k a -> zip (repeat k) (atomTransitions k a)) statesPairs
    atomTransitions i a = IM.keys $ IM.filter (hasTransition a) statesPairs

    hasTransition (s1,k1) (s2,k2) =
      let
        ts = stateTransitions s1 model
        tk = stateTransitions k1 autom
      in
        s2 `elem` (map Logic.target ts) && k2 `elem` (map Logic.target tk)

    stateTransitions s ks = filter (\t -> Logic.source t == s) $ Logic.transitions ks


findCompatibleStates :: [Logic.State String] -> [Logic.State Expr] -> IntMap (Int, Int)
findCompatibleStates modelStates automatonStates = IM.fromList $ zip [0..] compatiblePairs
  where
    compatiblePairs = concat $ map (\s -> zip (repeat $ Logic.elementId s) (getCompatibleASts s)) modelStates
    getCompatibleASts s = map Logic.elementId $ filter (statesAreCompatible s) automatonStates
    statesAreCompatible (Logic.State _ ps) (Logic.State _ exprs) = containsAtoms && dontContainNegativeAtoms
      where
        containsAtoms = and $ map (\p -> p `elem` ps) (extractAtoms exprs)
        dontContainNegativeAtoms = and $ map (\p -> p `notElem` ps) (extractNegativeAtoms exprs)

        extractAtoms :: [Expr] -> [String]
        extractAtoms [] = []
        extractAtoms ((Atom p):ps) = p : (extractAtoms ps)
        extractAtoms (_:ps) = extractAtoms ps

        extractNegativeAtoms :: [Expr] -> [String]
        extractNegativeAtoms [] = []
        extractNegativeAtoms ((Not(Atom p)):ps) = p : (extractNegativeAtoms ps)
        extractNegativeAtoms (_:ps) = extractNegativeAtoms ps
