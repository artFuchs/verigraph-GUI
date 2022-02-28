{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Logic.Ltl.Parser (parseExpr) where

import           Data.Functor.Identity
import           Text.Parsec
import qualified Text.Parsec.Token     as P

import           Logic.Ltl.Base

type Parser a = Parsec String () a

-- | Parse LTL expressions from the given string
parseExpr :: SourceName -> String -> Either ParseError Expr
parseExpr =
  parse (whiteSpace *> expr <* eof)

expr :: Parser Expr
expr =
  exprImplicative

exprImplicative :: Parser Expr
exprImplicative =
  do
    e1 <- exprBoolean
    exprImplies e1 <|> exprEquiv e1 <|> pure e1
  where
    exprImplies =
      exprAssocRight exprBoolean (reservedOp "->") Implies

    exprEquiv e1 =
      Equiv e1 <$> (reservedOp "<->" *> exprBoolean)

exprBoolean :: Parser Expr
exprBoolean =
  do
    e1 <- exprUnary
    exprAnd e1 <|> exprOr e1 <|> pure e1
  where
    exprAnd =
      exprAssocRight exprUnary (reservedOp "&&") And

    exprOr =
      exprAssocRight exprUnary (reservedOp "||") Or

exprAssocRight :: Parser a -> Parser b -> (a -> a -> a) -> a -> Parser a
exprAssocRight item sep combine =
  let
    go e1 =
      do
        e2 <- sep *> item
        combine e1 <$> (go e2 <|> pure e2)
  in
    go

exprUnary :: Parser Expr
exprUnary =
  exprNot <|> exprUnaryTemporal <|> exprBinaryTemporal

exprNot, exprUnaryTemporal, exprBinaryTemporal :: Parser Expr
exprNot =
  Not <$> (reservedOp "~" *> exprUnary)

exprUnaryTemporal =
  Temporal <$> (unaryStateQuantifier <*> exprUnary)

exprBinaryTemporal =
  Temporal <$> (exprInfix expr binaryStateQuantifier)

unaryStateQuantifier :: Parser (e -> StateQuantified e)
unaryStateQuantifier =
  (reserved "X" *> pure X)
  <|> (reserved "F" *> pure F)
  <|> (reserved "G" *> pure G)
  <?> "temporal connective"

binaryStateQuantifier :: Parser (e -> e -> StateQuantified e)
binaryStateQuantifier =
  (reserved "U" *> pure U)
  <|> (reserved "W" *> pure W)
  <|> (reserved "R" *> pure R)
  <?> "temporal connective"



exprInfix :: Parser a -> Parser (a -> a -> b) -> Parser b
exprInfix item operator =
  do
    e1 <- item
    op <- operator
    e2 <- item
    return (e1 `op` e2)

exprAtomic :: Parser Expr
exprAtomic =
  literal <|> atom <|> parens expr



atom, literal :: Parser Expr
atom =
  Atom <$> identifier <?> "atomic formula"

literal =
  let
    true =
      reserved "true"  *> pure True <?> "true"

    false =
      reserved "false" *> pure False <?> "false"
  in
    Literal <$> (true <|> false)

brackets, parens :: Parser a -> Parser a
brackets =
  P.brackets lexer

parens =
  P.parens lexer


reserved, reservedOp :: String -> Parser ()
reserved =
  P.reserved lexer

reservedOp =
  P.reservedOp lexer


identifier :: Parser String
identifier =
  P.identifier lexer


whiteSpace :: Parser ()
whiteSpace =
  P.whiteSpace lexer

lexer :: (Stream s Identity Char) => P.GenTokenParser s u Identity
lexer =
  P.makeTokenParser ltlDef

ltlDef :: (Stream s Identity Char) => P.GenLanguageDef s u Identity
ltlDef =
  P.LanguageDef
    { P.commentStart =
        "{-"

    , P.commentEnd =
        "-}"

    , P.commentLine =
        "--"

    , P.nestedComments =
        True

    , P.identStart =
        lower <|> oneOf "_"

    , P.identLetter =
        alphaNum <|> oneOf "_'"

    , P.opStart =
        oneOf "&|-<~"

    , P.opLetter =
        oneOf "&|->"

    , P.reservedOpNames =
        ["&&", "||", "~", "->", "<->"]

    , P.reservedNames =
        ["true", "false", "X", "F", "G", "U", "W", "R"]

    , P.caseSensitive =
        True
    }
