module Logic.Ltl.RewriteExprSpec where


import           Test.Hspec
import qualified Test.HUnit              as HUnit

import           Logic.Ltl.Base
import           Logic.Ltl.Parser
import           Logic.Ltl.Semantics

import Data.Set (Set)
import qualified Data.Set as Set

import           Logic.Ltl.TestUtils     ()


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

    it "rewrites '~a'" $
      "~a" `shouldRewriteTo` (Not$Atom "a")

    it "rewrites '~~a'" $
      "~~a" `shouldRewriteTo` Atom "a"

    it "rewrites 'a && b'" $
      "a && b" `shouldRewriteTo` (Atom "a" `And` Atom "b")

    it "rewrites 'a || b'" $
      "a || b" `shouldRewriteTo` (Atom "a" `Or` Atom "b")

    it "rewrites 'a -> b'" $
      "a -> b" `shouldRewriteTo` (Atom "a" `Implies` Atom "b")

    it "rewrites 'a <-> b'" $
      "a <-> b" `shouldRewriteTo` (Atom "a" `Equiv` Atom "b")

  context "rewrites negated temporal connectors" $ do
    it "rewrites '~(F a)'" $
      "~(F a)" `shouldRewriteTo` (Not (Temporal$ (Literal True) `U` (Atom "a")))

    it "rewrites '~(G foo)'" $
      "~(G foo)" `shouldRewriteTo` (Temporal$ (Literal True) `U` (Not$Atom "foo"))

    it "rewrites '~(foo U bar)'" $
      "~(foo U bar)" `shouldRewriteTo` (Not (Temporal$ (Atom "foo") `U` (Atom "bar")))

    it "rewrites '~(foo W bar)'" $
      "~(foo W bar)" `shouldRewriteTo` (Not (Or (Temporal$ Atom "foo" `U` Atom "bar")
                                      (Not (Temporal$ (Literal True) `U` (Not$Atom "foo")))))

    it "rewrites '~(foo R bar)'" $
      "~(foo R bar)" `shouldRewriteTo` (Temporal$ (Not$Atom "foo") `U` (Not$Atom "bar"))



shouldRewriteTo :: String -> Expr -> HUnit.Assertion
shouldRewriteTo text expected =
  case parseExpr "" text of
    Right expr ->
      let actual = rewriteExpr expr
      in HUnit.assertEqual ("When rewriting '" ++ text ++ "'") expected actual
    Left err ->
      HUnit.assertFailure ("Parse error for '" ++ text ++ "':\n" ++ show err)
