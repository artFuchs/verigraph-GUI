module Logic.Ltl.ModelCheckerSpec where


import           Test.Hspec
import qualified Test.HUnit              as HUnit

import           Logic.Ltl.Base
import           Logic.Ltl.Parser
import           Logic.Ltl.Semantics


spec :: Spec
spec = do
  context "temporal connectors" $ do
    it "rewrites 'X foo'" $
      "X foo" `shouldRewriteTo` (Temporal$X$ Atom "foo")

    it "rewrites 'F foo'" $
      "F foo" `shouldRewriteTo` (Temporal$ Literal True `U` Atom "foo")

    it "rewrites 'G foo'" $
      "G foo" `shouldRewriteTo` (Not (Temporal$ (Not$Literal False) `U` (Not$Atom "foo")))

    it "rewrites 'foo U bar'" $
      "foo U bar" `shouldRewriteTo` (Temporal$ Atom "foo" `U` Atom "bar")

    it "rewrites 'foo W bar'" $
      "foo W bar" `shouldRewriteTo` (Or (Temporal$ Atom "foo" `U` Atom "bar")
                                      (Not (Temporal$ (Not$Literal False) `U` (Not$Atom "foo"))))

    it "rewrites 'foo R bar'" $
      "foo R bar" `shouldRewriteTo` (Not (Temporal$ (Not$Atom "foo") `U` (Not$Atom "bar")))



shouldRewriteTo :: String -> Expr -> HUnit.Assertion
shouldRewriteTo text expected =
  case parseExpr "" text of
    Right expr ->
      let actual = rewriteExpr expr
      in HUnit.assertEqual ("When parsing '" ++ text ++ "'") expected actual
    Left err ->
      HUnit.assertFailure ("Parse error for '" ++ text ++ "':\n" ++ show err)
