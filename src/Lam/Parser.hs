module Lam.Parser where

import Text.Parsec
import Text.Parsec.Expr

import Val
import Types hiding (letin,parens,int)

import Lam.Lexer
import Lam.Syntax

parseLam' :: StreamP -> Lam
parseLam' s = case parseLam s of
  Right exp -> exp
  Left err -> error $ show err

parseLam :: StreamP -> Either ParseError Lam
parseLam = parse (between whiteSpace eof expr) ""

expr :: Parser Lam
expr = buildExpressionParser table term

table :: OperatorTable StreamP UserStateP MonadP Lam
table = [[binary ""  App AssocLeft]
        ,[binary "+" Add AssocLeft]]
  where binary  op f = Infix   $ reservedOp op >> return f
        prefix  op f = Prefix  $ reservedOp op >> return f
        postfix op f = Postfix $ reservedOp op >> return f

term :: Parser Lam
term = choice
  [ try val
  , try lam
  , try letin
  , parens expr
  ]

val :: Parser Lam
val = Val <$> choice
  [ try var
  , int
  ]

lam :: Parser Lam
lam = do
  reservedOp "\\"
  args <- many1 identifier
  reservedOp "->"
  body <- expr
  return $ foldr Lam body args

letin :: Parser Lam
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
