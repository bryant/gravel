module Gravel where

import Text.Parsec ((<|>), (<?>))
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as PExp
import qualified Text.Parsec.Token as Tok
import Control.Applicative ((<$>), (<*>), (<*))
import Control.Monad (guard)

type ParserState = P.Column

getIndentation :: P.Parsec s ParserState (P.Column, P.Column)
getIndentation = do
    base <- P.getState
    cur <- P.sourceColumn <$> P.getPosition
    return (cur, base)

indented = getIndentation >>= guard . (uncurry (>)) <?> "indentation"

sameIndent = getIndentation >>= guard . (uncurry (==)) <?> "no indentation"

baseIndent p = do
    (cur, base) <- getIndentation
    r <- P.putState cur >> p <* P.putState base
    return r

data Module = Module [TopLevelDecl] deriving Show

data TopLevelDecl =
    TopFuncDecl FuncDecl |
    TopVarDecl VarDecl
    deriving Show

type Ident = String

data FuncDecl =
    ExternFuncDecl Type Ident [VarDecl] |
    FuncDecl Type Ident [VarDecl] [Statement]
    deriving Show

data VarDecl = VarDecl Ident (Maybe Type) (Maybe Expression) deriving Show

data Statement =
    VarDeclStatement VarDecl |
    Assignment Ident Expression |
    Return (Maybe Expression) |
    If Expression [Statement] |
    While Expression [Statement] |
    ExprStatement Expression
    deriving Show

type Type = String

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
    FuncCall Ident [Expression]
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
    Tok.identStart = P.letter <|> P.char '_',
    Tok.identLetter = P.alphaNum <|> P.char '_',
    Tok.opStart = P.parserZero,
    Tok.opLetter = P.parserZero,
    Tok.caseSensitive = True
}

intLit = IntLiteral <$> Tok.natural tokp

floatLit = FloatLiteral <$> Tok.float tokp

boolLit = BoolLiteral <$> bool'
    where bool' = (Tok.reserved tokp "True" >> return True) <|>
                  (Tok.reserved tokp "False" >> return False)

strLit = StringLiteral <$> Tok.stringLiteral tokp

varExpr = Variable <$> Tok.identifier tokp

atom :: P.Parsec String ParserState Expression
atom = P.choice $ map P.try [floatLit, intLit, boolLit, strLit, varExpr,
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

typeDecl = P.choice $ map (Tok.symbol tokp) ["u32", "i32"]

varDecl' = VarDecl <$> Tok.identifier tokp <*> (Just <$> typeDecl)

varDeclStatement = varDecl' <*> P.optionMaybe (Tok.reservedOp tokp "=" >> expr)

statement = P.choice [
    VarDeclStatement <$> varDeclStatement,
    Assignment <$> Tok.identifier tokp <*> (P.char '=' >> expr),
    ExprStatement <$> expr,
    Return <$> P.optionMaybe (Tok.reserved tokp "return" >> expr)
    ]

statementBlock :: P.Parsec String ParserState [Statement]
statementBlock = indented >> baseIndent (P.many1 $ sameIndent >> statement)

funcParam = varDecl' <*> return Nothing

funcDecl = FuncDecl <$> typeDecl <*> Tok.identifier tokp <*> params <*>
           (colon >> statementBlock)
    where
    params = Tok.parens tokp $ funcParam `P.sepBy` commas
    colon = Tok.lexeme tokp $ P.char ':'
    commas = Tok.lexeme tokp $ P.char ','

parseTopLevel = P.choice [
    TopFuncDecl <$> funcDecl,
    TopVarDecl <$> varDeclStatement
    ]

parseModule = Module <$> (Tok.whiteSpace tokp >> baseIndent (P.many1 topLevels)) <* P.eof
    where topLevels = sameIndent >> parseTopLevel

testParse = P.runParser parseModule 0 ""
