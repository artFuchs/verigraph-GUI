module Logic.Ltl.AutomatonGeneration where


import           Test.Hspec
import qualified Test.HUnit              as HUnit

import           Logic.Ltl.Base
import           Logic.Ltl.Parser
import           Logic.Ltl.Automaton
import           Logic.Model

import Data.Set (Set)
import qualified Data.Set as Set


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



closureShouldBe ::  String -> Closure -> HUnit.Assertion
closureShouldBe text expected =
  case parseExpr "" text of
    Right expr ->
      let actual = closure expr
      in HUnit.assertEqual ("When generating closure for'" ++ text ++ "'") expected actual
    Left err ->
      HUnit.assertFailure ("Parse error for '" ++ text ++ "':\n" ++ show err)
