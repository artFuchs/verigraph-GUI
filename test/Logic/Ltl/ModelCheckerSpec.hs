module Logic.Ltl.ModelCheckerSpec where


import           Test.Hspec
import qualified Test.HUnit              as HUnit

import           Logic.Ltl.Base
import           Logic.Ltl.Parser
import           Logic.Ltl.Semantics

import Data.Set (Set)
import qualified Data.Set as Set


spec :: Spec
spec = do
  context "rewriting temporal connectors" $ do
    it "rewrites 'X foo'" $
      "X foo" `shouldRewriteTo` (Temporal$X$ Atom "foo")

    it "rewrites 'F foo'" $
      "F foo" `shouldRewriteTo` (Temporal$ Literal True `U` Atom "foo")

    it "rewrites 'G foo'" $
      "G foo" `shouldRewriteTo` (Not (Temporal$ (Literal True) `U` (Not$Atom "foo")))

    it "rewrites 'foo U bar'" $
      "foo U bar" `shouldRewriteTo` (Temporal$ Atom "foo" `U` Atom "bar")

    it "rewrites 'foo W bar'" $
      "foo W bar" `shouldRewriteTo` (Or (Temporal$ Atom "foo" `U` Atom "bar")
                                      (Not (Temporal$ (Literal True) `U` (Not$Atom "foo"))))

    it "rewrites 'foo R bar'" $
      "foo R bar" `shouldRewriteTo` (Not (Temporal$ (Not$Atom "foo") `U` (Not$Atom "bar")))

  context "rewriting propositional connectors" $ do
    it "rewrites '~true'" $
      "~true" `shouldRewriteTo` (Literal False)

    it "rewrites '~false'" $
      "~false" `shouldRewriteTo` (Literal True)

    it "rewrites 'a && b'" $
      "a && b" `shouldRewriteTo` (Atom "a" `And` Atom "b")

    it "rewrites 'a || b'" $
      "a || b" `shouldRewriteTo` (Atom "a" `Or` Atom "b")

    it "rewrites 'a -> b'" $
      "a -> b" `shouldRewriteTo` (Atom "a" `Implies` Atom "b")

    it "rewrites 'a <-> b'" $
      "a <-> b" `shouldRewriteTo` (Atom "a" `Equiv` Atom "b")

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

shouldRewriteTo :: String -> Expr -> HUnit.Assertion
shouldRewriteTo text expected =
  case parseExpr "" text of
    Right expr ->
      let actual = rewriteExpr expr
      in HUnit.assertEqual ("When rewriting '" ++ text ++ "'") expected actual
    Left err ->
      HUnit.assertFailure ("Parse error for '" ++ text ++ "':\n" ++ show err)

closureShouldBe ::  String -> Closure -> HUnit.Assertion
closureShouldBe text expected =
  case parseExpr "" text of
    Right expr ->
      let actual = closure expr
      in HUnit.assertEqual ("When generating closure for'" ++ text ++ "'") expected actual
    Left err ->
      HUnit.assertFailure ("Parse error for '" ++ text ++ "':\n" ++ show err)
