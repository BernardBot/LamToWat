module Lam.Parser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Indent

import Types hiding (letin,parens)

import Lam.Lexer
import Lam.Syntax

parseExpr' :: StreamP -> Expr
parseExpr' s = case parseExpr s of
  Right exp -> exp
  Left err -> error $ show err

parseExpr :: StreamP -> Either ParseError Expr
parseExpr = runIndentParser (between whiteSpace eof expr) () ""

expr :: Parser Expr
expr = buildExpressionParser table term

table :: OperatorTable StreamP UserStateP MonadP Expr
table = [[binary ""  App AssocLeft]
        ,[binary "+" Add AssocLeft]]
  where binary  op f = Infix   $ reservedOp op >> return f
        prefix  op f = Prefix  $ reservedOp op >> return f
        postfix op f = Postfix $ reservedOp op >> return f

term :: Parser Expr
term = choice
  [ try var
  , try num
  , try lam
  , try letin
  , parens expr
  ]

var :: Parser Expr
var = Var <$> identifier

num :: Parser Expr
num = Num <$> fromInteger <$> integer

lam :: Parser Expr
lam = do
  reservedOp "\\"
  args <- many1 identifier
  reservedOp "->"
  body <- expr
  return $ foldr Lam body args

letin :: Parser Expr
letin = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return $ App (Lam x e2) e1
