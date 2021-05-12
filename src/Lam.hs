{-# LANGUAGE DeriveDataTypeable #-}

module Lam where

import Data.Tree

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr

import Val
import Interpreter
import Types

data Lam
  = Lam Var Lam
  | App Lam Lam
  | Add Lam Lam
  | Val Val
  deriving (Eq,Show)

instance Interpretable Lam where
  interp (Val v) = interp v
  interp (Lam x e) = closure [x] (interp e)
  interp (App e1 e2) = do
    Fun f <- interp e1
    a <- interp e2
    f [a]
  interp (Add e1 e2) = do
    Int i <- interp e1
    Int j <- interp e2
    int $ i + j


instance Treeable Lam where
  toTree (Lam x e) = Node ("Lam " ++ show x) [toTree e]
  toTree (App e1 e2) = Node "App" [toTree e1, toTree e2]
  toTree (Add e1 e2) = Node "Add" [toTree e1, toTree e2]
  toTree (Val v) = toTree v
  
------------
-- Parser --
------------

str2lam :: String -> Lam
str2lam str = case parseLam str of
  Left err -> error $ show err
  Right exp -> exp

parseLam :: String -> Either ParseError Lam
parseLam = parse (between whiteSpace eof expr) ""
  where expr = buildExpressionParser

          [ [binary ""  App AssocLeft]
          , [binary "+" Add AssocLeft]
          ] $

          choice [ try val
                 , try lam
                 , try letin
                 , parens expr
                 ]

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
