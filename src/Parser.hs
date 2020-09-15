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
  [ [binaryOp "*" OpMul, binaryOp "/" OpDiv],
    [binaryOp "+" OpPlus, binaryOp "-" OpMinus],
    [ binaryRelOp "=" OpEq,
      binaryRelOp "!=" OpNeq,
      binaryRelOp "<" OpLess,
      binaryRelOp "<=" OpLessEq,
      binaryRelOp ">" OpGreater,
      binaryRelOp ">=" OpGreaterEq
    ],
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
    -- as parsing a single operator
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

pExpr :: Parser Expr
pExpr = buildExpressionParser operatorTable pFac <?> "expression"

pFac :: Parser Expr
pFac =
  choice
    [ pNegOpExpr,
      pNotOpExpr,
      parens pExpr,
      pNum,
      pBool,
      pString,
      pLvalExpr
    ]
    <?> "simple expression"

pNum :: Parser Expr
pNum =
  do
    n <- natural <?> ""
    return $ ConstInt (fromInteger n)
    <?> "number"

pBool :: Parser Expr
pBool =
  reserved "true" $> ConstBool True
    <|> reserved "false" $> ConstBool False
    <?> "boolean"

pString :: Parser Expr
pString =
  do
    char '"'
    str <- many (normalChar <|> escapedChar <?> "allowed character")
    symbol "\"" <?> "end of string"
    return $ ConstStr (concat str)
    <?> "string"
  where
    escapedChar =
      do
        bs <- char '\\'
        chr <- anyChar
        return [bs, chr]
        <?> ""

    -- match any single char other than disallowed whitespace, double quotes, or backslashes
    normalChar =
      do
        c <- noneOf ['\\', '"', '\t', '\n']
        return [c]
        <?> ""

pLvalExpr :: Parser Expr
pLvalExpr = LVal <$> pLval <?> "identifier"

pLval :: Parser LValue
pLval =
  do
    ident <- identifier
    index <- optionMaybe (squares pExpr) <?> "index expression"
    field <- optionMaybe (dot *> identifier) <?> "field"
    return $ LValue ident index field
    <?> "lvalue"

pNegOpExpr :: Parser Expr
pNegOpExpr = symbol "-" $> UnOpExpr OpNeg <*> pFac <?> ""

pNotOpExpr :: Parser Expr
pNotOpExpr = reserved "not" $> UnOpExpr OpNot <*> pFac <?> ""

--
-- Statements
--

pStmt :: Parser Stmt
pStmt =
  pCompositeStmt <|> pAtomicStmt
    <?> "statement"

pCompositeStmt :: Parser Stmt
pCompositeStmt = SComp <$> choice [pIf, pWhile] <?> ""

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
pCall = reserved "call" *> liftA2 Call identifier pArgs <?> "call"
  where
    pArgs = parens $ sepBy pExpr (symbol ",")

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
  reserved "record" *> liftA2 RecordDef pFields identifier <* semi <?> "record definition"
  where
    pFields = braces $ sepBy1 pField semi

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
  liftA2 ProcHead identifier (parens $ pProcParam `sepBy` symbol ",")
    <?> "procedure header"

pProcBody :: Parser ProcBody
pProcBody = liftA2 ProcBody (many pVarDecl) (braces $ many1 pStmt) <?> "procedure body"

pProcParam :: Parser ProcParam
pProcParam = liftA2 ProcParam pProcParamType identifier <?> "parameter"

pProcParamType :: Parser ProcParamType
pProcParamType =
  ParamBuiltinT <$> pBuiltinType <*> tryParsePassType
    <|> ParamAliasT <$> identifier
    <?> "parameter type"
  where
    tryParsePassType = option PassByRef (reserved "val" $> PassByVal)

pVarDecl :: Parser VarDecl
pVarDecl =
  do
    varType <-
      VarBuiltinT <$> pBuiltinType
        <|> VarAliasT <$> identifier
        <?> "type"
    idents <- sepBy1 identifier $ symbol ","
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
