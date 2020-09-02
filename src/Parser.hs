module Parser where

import AST
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
    [binaryRelTextOp "and" OpAnd],
    [binaryRelTextOp "or" OpOr]
  ]
  where
    defBinaryOp fixity assoc parseOp name op =
      fixity
        ( do
            parseOp name
            return $ BinOpExpr op
        )
        assoc

    binaryOp = defBinaryOp Infix AssocLeft reservedOp
    binaryRelOp = defBinaryOp Infix AssocNone reservedOp
    binaryRelTextOp = defBinaryOp Infix AssocNone reserved

--
-- Expressions
--

parseExpr :: Parser Expr
parseExpr = buildExpressionParser operatorTable parseFac <?> "expression"

parseFac :: Parser Expr
parseFac =
  choice [parens parseExpr, parseNum, parseBool, parseString, parseIdent]
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
parseString = fail "unimplemented" -- TODO

parseIdent :: Parser Expr
parseIdent = do LVal <$> parseLval <?> "identifier"

parseLval :: Parser LValue
parseLval =
  do
    ident <- identifier
    index <- optionMaybe $ squares parseExpr -- this allows spaces - should it?
    field <- optionMaybe $ dot *> identifier
    return $ LValue ident index field
    <?> "lvalue"

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
  BoolField <$> (reserved "boolean" *> identifier)
    <|> IntField <$> (reserved "integer" *> identifier)
    <?> "field declaration"

--
-- Arrays
--

--

parse' :: Parser p -> String -> Either ParseError p
parse' p = runParser p 0 ""
