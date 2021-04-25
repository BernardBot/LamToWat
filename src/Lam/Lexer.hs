module Lam.Lexer where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import Types

lexer :: GenTokenParser StreamP UserStateP MonadP
lexer = makeTokenParser style

style :: GenLanguageDef StreamP UserStateP MonadP
style = emptyDef
        { reservedOpNames = ["\\","->","="]
        , reservedNames   = ["let","in"]
        }

identifier     = Text.Parsec.Token.identifier     lexer
reserved       = Text.Parsec.Token.reserved       lexer
reservedOp     = Text.Parsec.Token.reservedOp     lexer
integer        = Text.Parsec.Token.integer        lexer
whiteSpace     = Text.Parsec.Token.whiteSpace     lexer
parens         = Text.Parsec.Token.parens         lexer
