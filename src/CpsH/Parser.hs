{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CpsH.Parser where

import Prelude hiding (exp)

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Indent

import Types hiding (parens,int,record,fix)

import CpsH.Lexer
import CpsH.Syntax

instance Parsable Expr where
  parseExpr = runIndentParser (between whiteSpace eof expr) () ""

expr :: Parser Expr
expr = fix

exp :: Parser Exp
exp = buildExpressionParser table term

table :: OperatorTable StreamP UserStateP MonadP Exp
table = []
  where binary  op f = Infix   $ reservedOp op >> return f
        prefix  op f = Prefix  $ reservedOp op >> return f
        postfix op f = Postfix $ reservedOp op >> return f

term :: Parser Exp
term = choice
  [ try record
  , try add
  , try select
  , try app
  , done
  ]

app :: Parser Exp
app = do
  fun <- val
  args <- parens (commaSep val)
  return $ APP fun args

done :: Parser Exp
done = do
  reserved "return"
  v <- val
  return $ DONE v

record :: Parser Exp
record = do
  x <- identifier
  reservedOp "="
  rs <- brackets (commaSep val)
  e <- exp
  return $ RECORD rs x e

select :: Parser Exp
select = do
  x <- identifier
  reservedOp "="
  v <- val
  i <- brackets (fromInteger <$> integer)
  e <- exp
  return $ SELECT i v x e

add :: Parser Exp
add = do
  x <- identifier
  reservedOp "="
  v1 <- val
  reservedOp "+"
  v2 <- val
  e <- exp
  return $ ADD v1 v2 x e

def :: Parser (Fun Exp)
def = do
  reserved "def"
  name <- identifier
  args <- parens (commaSep identifier)
  reservedOp ":"
  indented
  body <- exp
  return $ (name,args,body)
  
fix :: Parser Expr
fix = do
  funs <- many1 def
  e <- exp
  return $ (funs,e)

-- TODO: Val Parser in Types?

var :: Parser Val
var = VAR <$> identifier

int :: Parser Val
int = INT <$> fromInteger <$> integer

val :: Parser Val
val = choice
  [ try var
  , int
  ]
