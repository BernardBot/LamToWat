module Lam.Parser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Indent

import Types hiding (letin,parens,int)

import Lam.Lexer
import Lam.Syntax

instance Parsable Expr where
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
  [ try val
  , try lam
  , try letin
  , parens expr
  ]

val :: Parser Expr
val = Val <$> choice
  [ try var
  , int
  ]

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

-- Val parser

var :: Parser Val
var = VAR <$> identifier

int :: Parser Val
int = INT <$> fromInteger <$> integer
