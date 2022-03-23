module Logic.Ltl.AutomatonGenSpec where


import           Test.Hspec
import qualified Test.HUnit              as HUnit

import           Logic.Ltl.Base
import           Logic.Ltl.Parser
import           Logic.Ltl.Automaton
import           Logic.Model

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort)

import           Logic.Ltl.TestUtils     ()


spec :: Spec
spec = do
  context "generating closure for formulas" $ do
    it "generates closure for 'true'" $
      "true" `closureShouldBe` (Set.fromList [Literal False, Literal True])

    it "generates closure for 'a'" $
      "a" `closureShouldBe` (Set.fromList [Atom "a", Not $ Atom "a"])

    it "generates closure for 'X a'" $
      "X a" `closureShouldBe`
        (Set.fromList [
          Atom "a", Not $ Atom "a",
          Temporal (X $ Atom "a"), Not $ Temporal (X $ Atom "a")])

    it "generates closure for 'F a'" $
      "F a" `closureShouldBe`
        (Set.fromList [
          Literal False, Literal True,
          Atom "a", Not $ Atom "a",
          Temporal (Literal True `U` Atom "a"), Not $ Temporal (Literal True `U` Atom "a")])

    it "generates closure for 'a U b'" $
      "a U b" `closureShouldBe`
        (Set.fromList [
          Atom "a", Not $ Atom "a", Atom "b", Not $ Atom "b",
          Temporal (Atom "a" `U` Atom "b"), Not $ Temporal (Atom "a" `U` Atom "b")])

    it "generates closure for 'a && b'" $
      "a && b" `closureShouldBe`
        (Set.fromList [
          Atom "a", Not $ Atom "a", Atom "b", Not $ Atom "b",
          Atom "a" `And` Atom "b", Not (Atom "a" `And` Atom "b")])

    it "generates closure for 'a || b'" $
      "a || b" `closureShouldBe`
        (Set.fromList [
          Atom "a", Not $ Atom "a", Atom "b", Not $ Atom "b",
          Atom "a" `Or` Atom "b", Not (Atom "a" `Or` Atom "b")])

    it "generates closure for 'a -> b'" $
      "a -> b" `closureShouldBe`
        (Set.fromList [
          Atom "a", Not $ Atom "a", Atom "b", Not $ Atom "b",
          Atom "a" `Implies` Atom "b", Not (Atom "a" `Implies` Atom "b")])

    it "generates closure for 'a <-> b'" $
      "a <-> b" `closureShouldBe`
        (Set.fromList [
          Atom "a", Not $ Atom "a", Atom "b", Not $ Atom "b",
          Atom "a" `Equiv` Atom "b", Not (Atom "a" `Equiv` Atom "b")])

  context "generating states for expressions" $ do
    it "generates states for literals" $ do
      "true" `shouldGenerateStates` [[Literal True], [Literal False]]

    it "generates states for atomic propositions" $ do
      "a" `shouldGenerateStates` [[Atom "a"], [Not$Atom "a"]]

    it "generates states for '~a'" $ do
      "~a" `shouldGenerateStates` [[Atom "a"], [Not$Atom "a"]]

    it "generates states for 'X a'" $ do
      "X a" `shouldGenerateStates`
        [ [Temporal$X$Atom "a", Atom "a"]
        , [Temporal$X$Atom "a", Not$Atom "a"]
        , [Not$Temporal$X$Atom "a", Atom "a"]
        , [Not$Temporal$X$Atom "a", Not$Atom "a"]
        ]

    it "generates states for 'a U b'" $ do
      "a U b" `shouldGenerateStates`
        [ [Temporal$ Atom "a" `U` Atom "b", Atom "a", Atom "b"]
        , [Temporal$ Atom "a" `U` Atom "b", Atom "a", Not$Atom "b"]
        , [Temporal$ Atom "a" `U` Atom "b", Not$Atom "a", Atom "b"]
        , [Not$Temporal$ Atom "a" `U` Atom "b", Atom "a", Not$Atom "b"]
        , [Not$Temporal$ Atom "a" `U` Atom "b", Not$Atom "a", Not$Atom "b"]
        ]

closureShouldBe ::  String -> Closure -> HUnit.Assertion
closureShouldBe text expected =
  case parseExpr "" text of
    Right expr ->
      let actual = closure expr
      in HUnit.assertEqual ("When generating closure for'" ++ text ++ "'") expected actual
    Left err ->
      HUnit.assertFailure ("Parse error for '" ++ text ++ "':\n" ++ show err)

shouldGenerateStates :: String -> [[Expr]] -> HUnit.Assertion
shouldGenerateStates text expected =
  case parseExpr "" text of
    Right expr ->
      let actual = genStates expr
          expected' = (zipWith State [0..] $ sort $ map sort expected)
      in HUnit.assertEqual ("When generating states for'" ++ text ++ "'") expected' actual
    Left err ->
      HUnit.assertFailure ("Parse error for '" ++ text ++ "':\n" ++ show err)
