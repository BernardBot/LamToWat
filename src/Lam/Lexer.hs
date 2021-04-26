module Lam.Lexer where

import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language

import Types

lexer = Token.makeTokenParser style

style = emptyDef
        { Token.reservedOpNames = ["\\","->","=","+"]
        , Token.reservedNames   = ["let","in"]
        }

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
integer    = Token.integer    lexer
whiteSpace = Token.whiteSpace lexer
parens     = Token.parens     lexer
