-- should parse WebAssembly (richer than Wat)?
-- How do you do globals etc...?

module Wat.Parser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Indent

import Types hiding (letin,parens,int)

import Wat.Lexer
import Wat.Syntax

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
term = choice []
