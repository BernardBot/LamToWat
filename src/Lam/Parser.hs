module Lam.Parser where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr

import Val

import Lam.Syntax

str2lam :: String -> Lam
str2lam str = case parseLam str of
  Left err -> error $ show err
  Right exp -> exp

parseLam = parse (between whiteSpace eof expr) ""
  where expr = buildExpressionParser
          [ [binary ""  App AssocLeft]
          , [binary "+" Add AssocLeft]] $
          choice [ try val
                 , try lam
                 , try letin
                 , parens expr]

        binary op f = Infix $ reservedOp op >> return f

        val = Val <$> choice
          [ try $ VAR <$> identifier
          , INT <$> fromInteger <$> integer
          ]

        lam = do
          reservedOp "\\"
          args <- many1 identifier
          reservedOp "->"
          body <- expr
          return $ foldr Lam body args

        letin = do
          reserved "let"
          x <- identifier
          reservedOp "="
          e1 <- expr
          reserved "in"
          e2 <- expr
          return $ App (Lam x e2) e1

        lexer = makeTokenParser emptyDef
                { reservedOpNames = ["\\","->","=","+"]
                , reservedNames   = ["let","in"]
                }

        identifier = Text.Parsec.Token.identifier lexer
        reserved   = Text.Parsec.Token.reserved   lexer
        reservedOp = Text.Parsec.Token.reservedOp lexer
        integer    = Text.Parsec.Token.integer    lexer
        whiteSpace = Text.Parsec.Token.whiteSpace lexer
        parens     = Text.Parsec.Token.parens     lexer
