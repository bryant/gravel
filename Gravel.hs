module Gravel where

import Text.Parsec (Parsec, (<|>), parse, letter, char, alphaNum, oneOf)
import qualified Text.Parsec.Token as Tok
import Control.Applicative ((<$>), (<*>), (<*))

data Statement =
    VarDecl Ident Expression |
    Assignment Ident Expression |
    Return Expression |
    If Expression [Statement] |
    While Expression [Statement] |
    Expr Expression
    deriving Show

data UnaryOp =
    Negate |
    BitwiseNot |
    Not
    deriving Show

data BinaryOp =
    Exponent |
    Multiply |
    Divide |
    Modulo |
    Add |
    Subtract |
    LeftShift |
    RightShift |
    BitwiseAnd |
    Xor |
    BitwiseOr |
    Equal |
    NotEqual |
    GreaterOrEqual |
    LessOrEqual |
    Greater |
    Less |
    And |
    Or
    deriving Show

data Expression =
    IntLiteral String |
    FloatLiteral String |
    BoolLiteral Bool |
    StringLiteral String |
    Variable Ident |
    UnOp UnaryOp Expression |
    BinOp BinaryOp Expression Expression |
    AttributeRef Expression Ident |
    Subscript Expression Expression |
    FuncCall Ident [Expression]
    deriving Show

data Ident = Ident String deriving Show

reservedNames_ = [
    "while",
    "return",
    "if",
    "elif",
    "else",
    "u32",
    "i32"
    ]

reservedOpNames_ = [
    "-",
    "~",
    "not",

    "**",
    "*",
    "/",
    "%",
    "+",
    "<<",
    ">>",
    "&",
    "^",
    "|",
    "==",
    "!=",
    ">=",
    "<=",
    ">",
    "<",
    "and",
    "or",
    "="
    ]

tokp = Tok.makeTokenParser $ Tok.LanguageDef {
    Tok.reservedNames = reservedNames_,
    Tok.reservedOpNames = reservedOpNames_,
    Tok.commentLine = "#",
    Tok.commentStart = "",
    Tok.commentEnd = "",
    Tok.nestedComments = False,
    Tok.identStart = letter <|> char '_',
    Tok.identLetter = alphaNum <|> char '_',
    Tok.opStart = oneOf "",
    Tok.opLetter = oneOf "",
    Tok.caseSensitive = True
}

ident :: Parsec String u Ident
ident = Ident <$> Tok.identifier tokp
