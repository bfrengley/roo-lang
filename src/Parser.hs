-- |
-- Module: Parser
-- Description: This module defines the parser for a Roo program.
-- Maintainer: Stewart Webb <sjwebb@student.unimelb.edu.au>
--             Ben Frengley <bfrengley@student.unimelb.edu.au>
module Parser (parseRooProgram) where

import AST
import Control.Applicative (Applicative (liftA2))
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q

type Parser a =
  Parsec String Int a

rooScanner :: Q.TokenParser Int
rooScanner =
  Q.makeTokenParser
    ( emptyDef
        { Q.commentLine = "#",
          Q.nestedComments = True,
          Q.identStart = letter,
          Q.identLetter = alphaNum <|> oneOf "_'",
          Q.opStart = oneOf "=!<>+-*/",
          Q.opLetter = oneOf "=",
          Q.reservedNames = reservedWords,
          Q.reservedOpNames = operatorNames
        }
    )

whiteSpace :: Parser ()
whiteSpace = Q.whiteSpace rooScanner

natural :: Parser Integer
natural = Q.natural rooScanner

identifier :: Parser Ident
identifier = Ident <$> Q.identifier rooScanner

semi :: Parser String
semi = Q.semi rooScanner

comma :: Parser String
comma = Q.comma rooScanner

dot :: Parser String
dot = Q.dot rooScanner

parens :: Parser a -> Parser a
parens = Q.parens rooScanner

braces :: Parser a -> Parser a
braces = Q.braces rooScanner

squares :: Parser a -> Parser a
squares = Q.squares rooScanner

symbol :: String -> Parser String
symbol = Q.symbol rooScanner

reserved :: String -> Parser ()
reserved = Q.reserved rooScanner

reservedOp :: String -> Parser ()
reservedOp = Q.reservedOp rooScanner

commaSep :: Parser a -> Parser [a]
commaSep = Q.commaSep rooScanner

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Q.commaSep1 rooScanner

semiSep :: Parser a -> Parser [a]
semiSep = Q.semiSep rooScanner

semiSep1 :: Parser a -> Parser [a]
semiSep1 = Q.semiSep1 rooScanner

reservedWords :: [String]
reservedWords =
  [ "and",
    "array",
    "boolean",
    "call",
    "do",
    "else",
    "false",
    "fi",
    "if",
    "integer",
    "not",
    "od",
    "or",
    "procedure",
    "read",
    "record",
    "then",
    "true",
    "val",
    "while",
    "write",
    "writeln"
  ]

operatorNames :: [String]
operatorNames =
  [ "=",
    "!=",
    "<",
    "<=",
    ">",
    ">=",
    "+",
    "-",
    "*",
    "/",
    "<-",
    "and",
    "or",
    "not"
  ]

--
-- Operators
--

operatorTable :: [[Operator String Int Identity Expr]]
operatorTable =
  [ [chainableUnaryOp "-" OpNeg],
    [binaryOp "*" OpMul, binaryOp "/" OpDiv],
    [binaryOp "+" OpPlus, binaryOp "-" OpMinus],
    [ binaryRelOp "=" OpEq,
      binaryRelOp "!=" OpNeq,
      binaryRelOp "<" OpLess,
      binaryRelOp "<=" OpLessEq,
      binaryRelOp ">" OpGreater,
      binaryRelOp ">=" OpGreaterEq
    ],
    -- parsing of `not` is defined both here and later on
    -- this is for the general case, but it fails to parse `not` in expressions like `a = not b`
    [chainableUnaryOp "not" OpNot],
    [binaryOp "and" OpAnd],
    [binaryOp "or" OpOr]
  ]
  where
    -- A helper function for defining infix binary operators
    defBinaryOp assoc name op =
      Infix
        ( do
            reservedOp name
            return $ BinOpExpr op
        )
        assoc

    -- left-associative operators
    binaryOp = defBinaryOp AssocLeft
    -- non-associative operators
    binaryRelOp = defBinaryOp AssocNone

    -- this is a little obscure
    -- 'buildExpressionParser' doesn't support repeated prefix operators, so this works around that
    -- by using 'chainl1' we can greedily parse as many prefix operators as possible and treat it
    -- as parsing a single operator, even though it gives the right AST
    -- we only partially apply 'UnOpExpr', so we can just chain them together with `.`
    chainableUnaryOp name op =
      let compose = pure (.)
          pUnaryOp = reservedOp name $> UnOpExpr op
       in Prefix $ chainl1 pUnaryOp compose

-- 'pBuiltinType' parses one of the builtin type names, boolean or integer.
pBuiltinType :: Parser BuiltinType
pBuiltinType =
  (reserved "boolean" $> TBool <?> "boolean")
    <|> (reserved "integer" $> TInt <?> "integer")

--
-- Expressions
--

-- | 'pExpr' defines an expression parser based on the operators in 'operatorTable'.
pExpr :: Parser Expr
pExpr =
  buildExpressionParser operatorTable pFac
    <?> "expression"

-- | 'pFac' defines a parser for factors of expressions.
pFac :: Parser Expr
pFac =
  choice
    [ -- we define `not` parsing again here to handle the case of `not` on the RHS of a higher
      -- precedence binary operator, like in `a = not b`
      pNotOpExpr,
      parens pExpr,
      pNum,
      pBool,
      pString,
      pLvalExpr
    ]
    <?> "simple expression"

-- | 'pNum' defines a parser for constant integer expressions. 'pNum' only parses a non-negative
-- integer, as negation is handled by unary minus.
pNum :: Parser Expr
pNum =
  ConstInt <$> natural
    <?> "number"

-- | 'pBool' defines a parser for constant boolean expressions, i.e., `true` and `false`.
pBool :: Parser Expr
pBool =
  reserved "true" $> ConstBool True
    <|> reserved "false" $> ConstBool False
    <?> "boolean"

-- | 'pString' defines a parser for constant strings. A string consists of a pair of double quotes
-- surrounding any number of characters other than double quotes not preceded by a backslash, tabs,
-- or newlines. Escape sequences `\"`, `\t`, and `\n` are allowed instead. We additionally allow
-- `\\` to represent a backslash literal.
-- This parser parses escape sequences literally, and leaves the handling of them to the Oz virtual
-- machine. A string will thus preserve all backslashes, even if they are part of an escape sequence.
pString :: Parser Expr
pString =
  ConstStr <$> enquoted pString'
    <?> "string"
  where
    enquoted = between (char '"') (char '"')
    -- parse a sequence of string-legal characters
    pString' = concat <$> many (normalChar <|> escapeSeq <?> "allowed character")

    -- 'escapeSeq' matches a backslash followed by another character other than tab/newline
    -- the pair may not be an actual escape sequence, but we leave them intact regardless
    escapeSeq =
      do
        bs <- char '\\'
        -- all escape sequences are accepted, as interpreting them is left to the Oz VM
        chr <- noneOf ['\t', '\n']
        return [bs, chr]
        <?> ""

    -- match any single char other than disallowed whitespace, double quotes, or backslashes
    normalChar =
      do
        c <- noneOf ['\\', '"', '\t', '\n']
        return [c]
        <?> ""

-- | 'pLvalExpr' defines a parser for an lvalue expression. It is merely a wrapper around 'pLval'
-- that converts it to an expression AST node.
pLvalExpr :: Parser Expr
pLvalExpr =
  LVal <$> pLval

-- | 'pLval' defines a parser for an lvalue. An lvalue may have an index expression if the base
-- identifier is an array type (e.g., `a[1]`), a field access if the base identifier is a record
-- type (e.g., `a.b`), or both if the base identifier is an array type containing record types
-- (e.g., `a[1].b`).
pLval :: Parser LValue
pLval =
  do
    ident <- identifier
    index <- optionMaybe (squares pExpr) <?> "index expression"
    field <- optionMaybe (dot *> identifier) <?> "field"
    return $ LValue ident index field
    <?> "lvalue"

-- | 'pNotOpExpr' defines a parser for a `not` expression. This is only used to parse a `not` on
-- the right-hand side of a tightly binding binary operator (e.g., `a = not b`).
pNotOpExpr :: Parser Expr
pNotOpExpr = reservedOp "not" $> UnOpExpr OpNot <*> pFac <?> ""

--
-- Statements
--

-- | 'pStmt' defines a parser for a statement, either atomic or composite.
pStmt :: Parser Stmt
pStmt =
  pCompositeStmt <|> pAtomicStmt
    <?> "statement"

-- | 'pCompositeStmt' defines a parser for a composite statement (either `if` or `while`).
pCompositeStmt :: Parser Stmt
pCompositeStmt = SComp <$> choice [pIf, pWhile] <?> ""

-- | 'pAtomicStmt' defines a parser for an atomic statement.
pAtomicStmt :: Parser Stmt
pAtomicStmt =
  SAtom <$> choice [pAssign, pRead, pWrite, pWriteLn, pCall] <* semi <?> ""

pAssign :: Parser AtomicStmt
pAssign =
  Assign <$> (pLval <* reservedOp "<-") <*> pExpr
    <?> "assignment"

pRead :: Parser AtomicStmt
pRead =
  Read <$> (reserved "read" *> pLval)
    <?> "read"

pWrite :: Parser AtomicStmt
pWrite =
  Write <$> (reserved "write" *> pExpr)
    <?> "write"

pWriteLn :: Parser AtomicStmt
pWriteLn =
  WriteLn <$> (reserved "writeln" *> pExpr)
    <?> "writeln"

pCall :: Parser AtomicStmt
pCall =
  reserved "call" *> liftA2 Call identifier pArgs
    <?> "call"
  where
    pArgs = parens $ commaSep pExpr

pIf :: Parser CompositeStmt
pIf =
  do
    reserved "if"
    expr <- pExpr
    reserved "then"
    mainBody <- many1 pStmt
    elseBody <- option [] $ reserved "else" *> many1 pStmt
    reserved "fi"
    return $ IfBlock expr mainBody elseBody
    <?> "if block"

pWhile :: Parser CompositeStmt
pWhile =
  do
    reserved "while"
    expr <- pExpr
    reserved "do"
    body <- many1 pStmt
    reserved "od"
    return $ WhileBlock expr body
    <?> "while block"

--
-- Records
--

pRecordDef :: Parser RecordDef
pRecordDef =
  reserved "record" *> liftA2 RecordDef pFields identifier <* semi
    <?> "record definition"
  where
    pFields = braces $ semiSep1 pField

pField :: Parser FieldDecl
pField =
  FieldDecl <$> pBuiltinType <*> identifier
    <?> "field declaration"

--
-- Arrays
--

pArrayDef :: Parser ArrayDef
pArrayDef =
  do
    reserved "array"
    size <- squares pPosNum
    arrType <- pArrayType
    ident <- identifier
    semi
    return $ ArrayDef ident arrType size
    <?> "array definition"

pPosNum :: Parser Integer
pPosNum =
  try
    ( do
        n <- natural <?> ""
        if n > 0
          then return n
          else parserZero
    )
    <?> "positive number"

pArrayType :: Parser ArrayType
pArrayType =
  ArrBuiltinT <$> pBuiltinType
    <|> ArrAliasT <$> identifier
    <?> "array type"

--
-- Procedures
--

pProc :: Parser Procedure
pProc =
  reserved "procedure" *> liftA2 Procedure pProcHead pProcBody
    <?> "procedure"

pProcHead :: Parser ProcHead
pProcHead =
  liftA2 ProcHead identifier (parens $ commaSep pProcParam)
    <?> "procedure header"

pProcBody :: Parser ProcBody
pProcBody =
  liftA2 ProcBody (many pVarDecl) (braces $ many1 pStmt)
    <?> "procedure body"

pProcParam :: Parser ProcParam
pProcParam =
  liftA2 ProcParam pProcParamType identifier
    <?> "parameter"

pProcParamType :: Parser ProcParamType
pProcParamType =
  ParamBuiltinT <$> pBuiltinType <*> pPassMode
    <|> ParamAliasT <$> identifier
    <?> "parameter type"
  where
    pPassMode = option PassByRef (reserved "val" $> PassByVal)

pVarDecl :: Parser VarDecl
pVarDecl =
  do
    varType <-
      VarBuiltinT <$> pBuiltinType
        <|> VarAliasT <$> identifier
        <?> "type"
    idents <- commaSep1 identifier
    semi
    return $ VarDecl varType idents
    <?> "variable declaration"

--
-- Programs
--

pProgram :: Parser Program
pProgram =
  do
    records <- many pRecordDef
    arrays <- many pArrayDef
    procs <- many1 pProc
    return $ Program records arrays procs

parseRooProgram :: String -> Either ParseError Program
parseRooProgram = parse' pProgram

parse' :: Parser p -> String -> Either ParseError p
parse' p = runParser (whiteSpace *> p <* whiteSpace <* eof) 0 ""
