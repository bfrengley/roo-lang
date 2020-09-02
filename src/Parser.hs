module Parser where

import AST
import Control.Applicative (Applicative (liftA2))
import Data.Functor (($>))
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
identifier = Q.identifier rooScanner

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
    "<-"
  ]

--
-- Operators
--

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
    -- there's a bug here in that it doesn't support chained `not`, like `not not true`
    [unaryTextOp "not" OpNot],
    [binaryRelTextOp "and" OpAnd],
    [binaryRelTextOp "or" OpOr]
  ]
  where
    defBinaryOp assoc parseOp name op =
      Infix
        ( do
            parseOp name
            return $ BinOpExpr op
        )
        assoc

    binaryOp = defBinaryOp AssocLeft reservedOp
    binaryRelOp = defBinaryOp AssocNone reservedOp
    binaryRelTextOp = defBinaryOp AssocNone reserved
    unaryTextOp name op = Prefix (reservedOp name $> UnOpExpr op)

--
-- Expressions
--

parseExpr :: Parser Expr
parseExpr = buildExpressionParser operatorTable parseFac <?> "expression"

parseFac :: Parser Expr
parseFac =
  choice
    [ parseNegOpExpr,
      parens parseExpr,
      parseNum,
      parseBool,
      parseString,
      parseIdent
    ]
    <?> "simple expression"

parseNum :: Parser Expr
parseNum =
  do
    n <- natural <?> ""
    return $ ConstInt (fromInteger n)
    <?> "number"

parseBool :: Parser Expr
parseBool =
  do
    reserved "true"
    return $ ConstBool True
    <|> do
      reserved "false"
      return $ ConstBool False
    <?> "boolean"

parseString :: Parser Expr
parseString =
  do
    char '"'
    str <- many $ satisfy (/= '"')
    symbol "\""
    return $ ConstStr str
    <?> "string"

parseIdent :: Parser Expr
parseIdent = LVal <$> parseLval <?> "identifier"

parseLval :: Parser LValue
parseLval =
  do
    ident <- identifier
    index <- optionMaybe $ squares parseExpr
    field <- optionMaybe $ dot *> identifier
    return $ LValue ident index field
    <?> "lvalue"

parseNegOpExpr = symbol "-" $> UnOpExpr OpNeg <*> parseFac <?> ""

parseNotOpExpr = reserved "not" $> UnOpExpr OpNot <*> parseExpr <?> ""

--
-- Statements
--

parseStmt :: Parser Stmt
parseStmt =
  choice [parseAssign, parseRead, parseWrite, parseWriteLn, parseCall, parseIf, parseWhile]
    <?> "statement"

parseAssign :: Parser Stmt
parseAssign =
  Assign <$> (parseLval <* reservedOp "<-") <*> (parseExpr <* semi)
    <?> "assignment"

parseRead :: Parser Stmt
parseRead =
  Read <$> (reserved "read" *> parseLval <* semi)
    <?> "read"

parseWrite :: Parser Stmt
parseWrite =
  Write <$> (reserved "write" *> parseExpr <* semi)
    <?> "write"

parseWriteLn :: Parser Stmt
parseWriteLn =
  WriteLn <$> (reserved "writeln" *> parseExpr <* semi)
    <?> "writeln"

parseCall :: Parser Stmt
parseCall =
  do
    reserved "call"
    ident <- identifier
    exprs <- parens $ sepBy parseExpr (symbol ",")
    semi
    return $ Call ident exprs
    <?> "call"

parseIf :: Parser Stmt
parseIf =
  do
    reserved "if"
    expr <- parseExpr
    reserved "then"
    mainBody <- many1 parseStmt
    elseBody <- option [] $ reserved "else" *> many1 parseStmt
    reserved "fi"
    return $ IfBlock expr mainBody elseBody
    <?> "if block"

parseWhile :: Parser Stmt
parseWhile =
  do
    reserved "while"
    expr <- parseExpr
    reserved "do"
    body <- many1 parseStmt
    reserved "od"
    return $ WhileBlock expr body
    <?> "while block"

--
-- Records
--

parseRecordDef :: Parser RecordDef
parseRecordDef =
  do
    reserved "record"
    fields <- sepBy1 parseField semi
    ident <- identifier
    semi
    return $ RecordDef ident fields
    <?> "record definition"

parseField :: Parser FieldDecl
parseField =
  reserved "boolean" $> BoolField <*> identifier
    <|> reserved "integer" $> IntField <*> identifier
    <?> "field declaration"

--
-- Arrays
--

parseArrayDef :: Parser ArrayDef
parseArrayDef =
  do
    reserved "array"
    size <- fromInteger <$> squares natural
    arrType <- parseArrayType
    ident <- identifier
    semi
    return $ ArrayDef ident arrType size
    <?> "array definition"

parseArrayType :: Parser ArrayType
parseArrayType =
  reserved "boolean" $> BoolArr
    <|> reserved "integer" $> IntArr
    <|> AliasArr <$> identifier
    <?> "array type"

--
-- Procedures
--

parseProc :: Parser Procedure
parseProc =
  reserved "procedure" *> liftA2 Procedure parseProcHead parseProcBody
    <?> "procedure"

parseProcHead :: Parser ProcHead
parseProcHead =
  liftA2 ProcHead identifier (parens $ sepBy parseProcParam (symbol ","))
    <?> "procedure header"

parseProcBody :: Parser ProcBody
parseProcBody = liftA2 ProcBody (many parseVarDecl) (braces (many1 parseStmt)) <?> "procedure body"

parseProcParam :: Parser ProcParam
parseProcParam = liftA2 ProcParam identifier parseProcParamType <?> "parameter"

parseProcParamType :: Parser ProcParamType
parseProcParamType =
  reserved "boolean" $> BoolParam <*> tryParsePassType
    <|> reserved "integer" $> IntParam <*> tryParsePassType
    <|> AliasParam <$> identifier
    <?> "parameter type"
  where
    tryParsePassType = option PassByRef (reserved "val" $> PassByVal)

parseVarDecl :: Parser VarDecl
parseVarDecl =
  do
    varType <-
      reserved "boolean" $> BoolVar
        <|> reserved "integer" $> IntVar
        <|> AliasVar <$> identifier
        <?> "type"
    idents <- sepBy1 identifier $ symbol ","
    semi
    return $ VarDecl varType idents
    <?> "variable declaration"

--
-- Programs
--

parseRooProgram :: Parser Program
parseRooProgram =
  do
    whiteSpace
    records <- many parseRecordDef
    whiteSpace
    arrays <- many parseArrayDef
    whiteSpace
    procs <- many1 parseProc
    whiteSpace
    eof
    return $ Program records arrays procs

parse' :: Parser p -> String -> Either ParseError p
parse' p = runParser p 0 ""
