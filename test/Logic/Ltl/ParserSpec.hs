module Logic.Ltl.ParserSpec where


import           Test.Hspec
import qualified Test.HUnit              as HUnit
import           Test.QuickCheck

import           Logic.Ltl.Base
import           Logic.Ltl.Parser
import           Logic.Ltl.TestUtils     ()


spec :: Spec
spec = do
  context "with atomic expressions" $ do

    it "parses 'true'" $
      "true" `shouldParseTo` Literal True

    it "parses 'false'" $
      "false" `shouldParseTo` Literal False

    it "parses 'foo_bar1'" $
      "foo_bar1'" `shouldParseTo` Atom "foo_bar1'"

    it "parses '(foo)'" $
      "(foo)" `shouldParseTo` Atom "foo"

    it "parses '((true))'" $
      "((true))" `shouldParseTo` Literal True


  context "with unary expressions" $ do

    it "parses '~foo'" $
      "~foo" `shouldParseTo` (Not$ Atom "foo")

    it "parses 'X foo'" $
      "X foo" `shouldParseTo` (Temporal$X$ Atom "foo")

    it "parses 'F foo'" $
      "F foo" `shouldParseTo` (Temporal$F$ Atom "foo")

    it "parses 'G foo'" $
      "G foo" `shouldParseTo` (Temporal$G$ Atom "foo")

    it "parses 'F G foo'" $
      "F G foo" `shouldParseTo` (Temporal$F$ Temporal$G$ Atom "foo")

    it "parses '~X~foo'" $
      "~X~foo" `shouldParseTo` (Not$ Temporal$X$ Not$ Atom "foo")

    it "parses '(F (G foo))'" $
      "(F (G foo))" `shouldParseTo` (Temporal$F$ Temporal$G$ Atom "foo")

    it "parses '~((X~foo))'" $
      "~((X~foo))" `shouldParseTo` (Not$ Temporal$X$ Not$ Atom "foo")


  context "with binary boolean expressions" $ do

    it "parses 'foo && bar && baz'" $
      "foo && bar && baz" `shouldParseTo` And (Atom "foo") (Atom "bar" `And` Atom "baz")

    it "parses 'foo || bar || baz'" $
      "foo || bar || baz" `shouldParseTo` Or (Atom "foo") (Atom "bar" `Or` Atom "baz")

    it "parses '(foo || foo) && (foo || foo)'" $
      "(foo || foo) && (foo || foo)"
        `shouldParseTo` And (Atom "foo" `Or` Atom "foo") (Atom "foo" `Or` Atom "foo")

    it "parses '(foo && foo) || (foo && foo)'" $
      "(foo && foo) || (foo && foo)"
        `shouldParseTo` Or (Atom "foo" `And` Atom "foo") (Atom "foo" `And` Atom "foo")

    it "parses 'X( ~foo && F foo )'" $
      "X( ~foo && F foo )"
        `shouldParseTo` (Temporal$X$ (Not$ Atom "foo") `And` (Temporal$F$ Atom "foo"))

    it "does not parse 'foo && bar || baz'" $
      assertNoParse "foo && bar || baz"

    it "does not parse 'foo || bar && baz'" $
      assertNoParse "foo || bar && baz"


  context "with implicative expressions" $ do

    it "parses 'foo <-> bar'" $
      "foo <-> bar" `shouldParseTo` (Atom "foo" `Equiv` Atom "bar")

    it "parses 'foo -> bar -> baz'" $
      "foo -> bar -> baz" `shouldParseTo` (Atom "foo" `Implies` (Atom "bar" `Implies` Atom "baz"))

    it "parses '(foo -> bar) <-> ~foo || bar'" $
      "(foo -> bar) <-> ~foo || bar"
        `shouldParseTo` Equiv (Atom "foo" `Implies` Atom "bar") ((Not$ Atom "foo") `Or` Atom "bar")

    it "parses '(foo <-> bar) -> ~foo || bar'" $
      "(foo <-> bar) -> ~foo || bar"
        `shouldParseTo` Implies (Atom "foo" `Equiv` Atom "bar") ((Not$ Atom "foo") `Or` Atom "bar")

    it "parses '(foo -> bar) -> baz'" $
      "(foo -> bar) -> baz" `shouldParseTo` ((Atom "foo" `Implies` Atom "bar") `Implies` Atom "baz")

    it "parses '~(foo <-> bar)'" $
      "~(foo <-> bar)" `shouldParseTo` (Not$ Atom "foo" `Equiv` Atom "bar")

    it "parses 'AF((foo -> bar))'" $
      "F((foo -> bar))" `shouldParseTo` (Temporal$F$ Atom "foo" `Implies` Atom "bar")

    it "does not parse 'foo <-> bar <-> baz'" $
      assertNoParse "foo <-> bar <-> baz"

    it "does not parse 'foo <-> bar -> baz'" $
      assertNoParse "foo <-> bar -> baz"

    it "does not parse 'foo -> bar <-> baz'" $
      assertNoParse  "foo -> bar <-> baz"


  context "with binary temporal expressions" $ do

    it "parses 'foo U bar'" $
      "foo U bar" `shouldParseTo` (Temporal$ Atom "foo" `U` Atom "bar")

    it "parses 'foo W bar'" $
      "foo W bar" `shouldParseTo` (Temporal$ Atom "foo" `W` Atom "bar")

    it "parses 'foo R bar'" $
      "foo R bar" `shouldParseTo` (Temporal$ Atom "foo" `R` Atom "bar")

    it "parses 'foo U bar W foo R bar'" $
      "foo U bar W foo R bar"
        `shouldParseTo`
          (Temporal$ Atom "foo" `U` (Temporal$ Atom "bar" `W` (Temporal$ Atom "foo" `R` Atom "bar")))

    it "parses '(foo W bar) U (foo R bar)'" $
      "(foo W bar) U (foo R bar)"
        `shouldParseTo`
          (Temporal$U (Temporal$ Atom "foo" `W` Atom "bar")
                      (Temporal$ Atom "foo" `R` Atom "bar"))

    it "parses '(foo <-> bar) U (foo -> bar)'" $
      "(foo <-> bar) U (foo -> bar)"
        `shouldParseTo`
          (Temporal$U (Atom "foo" `Equiv` Atom "bar")
                      (Atom "foo" `Implies` Atom "bar"))

    it "parses 'F (foo U bar)'" $
      "F (foo U bar)" `shouldParseTo` (Temporal$F$ Temporal$ Atom "foo" `U` Atom "bar")



shouldParseTo :: String -> Expr -> HUnit.Assertion
shouldParseTo text expected =
  case parseExpr "" text of
    Right actual ->
      HUnit.assertEqual ("When parsing '" ++ text ++ "'") expected actual

    Left err ->
      HUnit.assertFailure ("Parse error for '" ++ text ++ "':\n" ++ show err)


assertNoParse :: String -> HUnit.Assertion
assertNoParse text =
  case parseExpr "" text of
    Right expr ->
      HUnit.assertFailure ("Expected '" ++ text ++ "' not to parse, but parsed to: \n" ++ show expr)

    Left  _ ->
      HUnit.assertBool "" True
