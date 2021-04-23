module Cps.Parser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Indent

import Types hiding (parens,int,record,fix)

import Cps.Lexer
import Cps.Syntax

parseExpr' :: StreamP -> Expr
parseExpr' s = case parseExpr s of
  Right exp -> exp
  Left err -> error $ show err

parseExpr :: StreamP -> Either ParseError Expr
parseExpr = runIndentParser (between whiteSpace eof expr) () ""

expr :: Parser Expr
expr = buildExpressionParser table term

table :: OperatorTable StreamP UserStateP MonadP Expr
table = []
  where binary  op f = Infix   $ reservedOp op >> return f
        prefix  op f = Prefix  $ reservedOp op >> return f
        postfix op f = Postfix $ reservedOp op >> return f

term :: Parser Expr
term = choice
  [ try record
  , try select
  , try add
  , try fix
  , try app
  , done
  ]

app :: Parser Expr
app = do
  fun <- val
  args <- parens (commaSep val)
  return $ APP fun args

done :: Parser Expr
done = do
  reserved "return"
  v <- val
  return $ DONE v

record :: Parser Expr
record = do
  x <- identifier
  reservedOp "="
  rs <- brackets (commaSep val)
  e <- expr
  return $ RECORD rs x e

select :: Parser Expr
select = do
  x <- identifier
  reservedOp "="
  v <- val
  i <- brackets (fromInteger <$> integer)
  e <- expr
  return $ SELECT i v x e

add :: Parser Expr
add = do
  x <- identifier
  reservedOp "="
  v1 <- val
  v2 <- val
  e <- expr
  return $ ADD v1 v2 x e

def :: Parser (Fun Expr)
def = do
  reserved "def"
  name <- identifier
  args <- parens (commaSep identifier)
  reservedOp ":"
  indented
  body <- expr
  return $ (name,args,body)
  
fix :: Parser Expr
fix = do
  funs <- many1 def
  e <- expr
  return $ FIX funs e

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
