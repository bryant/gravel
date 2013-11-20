module Gravel where

import Text.Parsec (Parsec, (<|>), parse, letter, char, alphaNum, oneOf,
                    parserZero, choice, try)
import qualified Text.Parsec.Expr as PExp
import qualified Text.Parsec.Token as Tok
import Control.Applicative ((<$>), (<*>), (<*))

data Statement =
    VarDecl String Expression |
    Assignment String Expression |
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
    AttrSel |
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
    IntLiteral Integer |
    FloatLiteral Double |
    BoolLiteral Bool |
    StringLiteral String |
    Variable String |
    UnOp UnaryOp Expression |
    BinOp BinaryOp Expression Expression |
    AttributeRef Expression String |
    Subscript Expression Expression |
    FuncCall String [Expression]
    deriving Show

reservedNames_ = words "while return if elif else u32 i32 True False"

reservedOpNames_ = words $ unary ++ " " ++ binary
    where
    unary = "- ~ not"
    binary = ". ** * / % + << >> & ^ | == != >= <= > < and or ="

tokp = Tok.makeTokenParser $ Tok.LanguageDef {
    Tok.reservedNames = reservedNames_,
    Tok.reservedOpNames = reservedOpNames_,
    Tok.commentLine = "#",
    Tok.commentStart = "",
    Tok.commentEnd = "",
    Tok.nestedComments = False,
    Tok.identStart = letter <|> char '_',
    Tok.identLetter = alphaNum <|> char '_',
    Tok.opStart = parserZero,
    Tok.opLetter = parserZero,
    Tok.caseSensitive = True
}

intLit = IntLiteral <$> Tok.natural tokp

floatLit = FloatLiteral <$> Tok.float tokp

boolLit = BoolLiteral <$> bool'
    where bool' = (Tok.reserved tokp "True" >> return True) <|>
                  (Tok.reserved tokp "False" >> return False)

strLit = StringLiteral <$> Tok.stringLiteral tokp

var = Variable <$> Tok.identifier tokp

atom :: Parsec String u Expression
atom = choice $ map try [floatLit, intLit, boolLit, strLit, var,
                         Tok.parens tokp expr]

expr = PExp.buildExpressionParser opPrecedence atom

opPrecedence = [
    [binOp "**" Exponent PExp.AssocRight],

    [unOp "-" Negate,
     unOp "~" BitwiseNot],

    [binOp "*" Multiply PExp.AssocLeft,
     binOp "/" Divide PExp.AssocLeft,
     binOp "%" Modulo PExp.AssocLeft],

    [binOp "+" Add PExp.AssocLeft,
     binOp "-" Subtract PExp.AssocLeft],

    [binOp "<<" LeftShift PExp.AssocLeft,
     binOp ">>" RightShift PExp.AssocLeft],

    [binOp "&" BitwiseAnd PExp.AssocLeft],

    [binOp "^" Xor PExp.AssocLeft],

    [binOp "|" BitwiseOr PExp.AssocLeft],

    [binOp "==" Equal PExp.AssocLeft,
     binOp "!=" NotEqual PExp.AssocLeft,
     binOp ">=" GreaterOrEqual PExp.AssocLeft,
     binOp "<=" LessOrEqual PExp.AssocLeft,
     binOp ">" Greater PExp.AssocLeft,
     binOp "<" Less PExp.AssocLeft],

    [unOp "not" Not],

    [binOp "and" And PExp.AssocLeft],

    [binOp "or" Or PExp.AssocLeft]
    ]
    where
    binOp op f = PExp.Infix $ Tok.reservedOp tokp op >> return (BinOp f)
    unOp op f = PExp.Prefix $ Tok.reservedOp tokp op >> return (UnOp f)
