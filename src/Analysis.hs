{-# LANGUAGE OverloadedStrings #-}

module Analysis where

import Control.Monad
import Control.Monad.State (runState)
import Text.Parsec (SourcePos)

import AST
import Semantics
import SymbolTable
import Util ((=>>))

-- Perform semantic analysis of the program and return the Symbol Tables for each procedure if there are no errors
analyseProgram :: Program -> Either [SemanticError] [SymbolTable]
analyseProgram prog@(Program _ _ procs) =
  let
    genTables = do
      globalTable <- buildGlobalSymbolTable prog
      localTables <- mapM (buildLocalSymbolTable globalTable) procs
      -- analyse every procedure to record any errors
      zipWithM_ analyseProcedure localTables procs
      return localTables
  in case runState genTables [] of
    (tables, []) -> Right tables
    (_, errs) -> Left $ reverse errs

analyseProcedure :: SymbolTable -> Procedure -> SemanticState ()
analyseProcedure table (Procedure _ (ProcBody _ stmts)) = mapM_ (analyseStmt table) stmts

analyseStmt :: SymbolTable -> Stmt -> SemanticState ()
analyseStmt table (SAtom pos stmt) = analyseAtomicStmt table pos stmt
analyseStmt table (SComp pos stmt) = analyseCompStmt table pos stmt

analyseCompStmt :: SymbolTable -> SourcePos -> CompositeStmt -> SemanticState ()
analyseCompStmt _ _ _ = return ()

analyseAtomicStmt :: SymbolTable -> SourcePos -> AtomicStmt -> SemanticState ()
analyseAtomicStmt table pos (Assign lval@(LValue _ ident _ _) expr) = do
  lvalT <- evaluateLvalType table lval
  evaluateExprType table expr >>= expectType lvalT (InvalidAssign pos ident)
analyseAtomicStmt _ _ _ = return ()

evaluateExprType :: SymbolTable -> Expr -> SemanticState LocalType
evaluateExprType _ (ConstInt _ _) = return intT
evaluateExprType _ (ConstBool _ _) = return boolT
evaluateExprType _ (ConstStr _ _) = return StringT
evaluateExprType table (UnOpExpr _ op expr) =
  evaluateExprType table expr =>> expectUnOpType op (getPos expr)
evaluateExprType table (BinOpExpr pos op left right) = do
  leftT <- evaluateExprType table left =>> expectBinOpType op (getPos left)
  rightT <- evaluateExprType table right =>> expectBinOpType op (getPos right)
  unless (leftT =%= rightT) $ addError $ BinaryTypeMismatch pos op leftT rightT
  return leftT -- this doesn't work if the left type is an invalid type
evaluateExprType table (LVal _ lval) = evaluateLvalType table lval

evaluateLvalType :: SymbolTable -> LValue -> SemanticState LocalType
evaluateLvalType table (LValue pos ident index field) = return UnknownT

boolT :: LocalType
boolT = BuiltinT TBool PassByVal

intT :: LocalType
intT = BuiltinT TInt PassByVal

expectType :: LocalType -> (LocalType -> LocalType -> SemanticError) -> LocalType -> SemanticState ()
expectType expected makeErr actual =
  unless (actual =%= expected) (addError $ makeErr actual expected)

expectTypes :: [LocalType] -> (LocalType -> [LocalType] -> SemanticError) -> LocalType -> SemanticState ()
expectTypes expected makeErr actual =
  unless (any (actual =%=) expected) (addError $ makeErr actual expected)

expectUnOpType :: UnaryOp -> SourcePos -> LocalType -> SemanticState ()
expectUnOpType OpNot pos = expectType boolT (InvalidUnaryType pos OpNot)
expectUnOpType OpNeg pos = expectType intT (InvalidUnaryType pos OpNeg)

expectBinOpType :: BinaryOp -> SourcePos -> LocalType -> SemanticState ()
expectBinOpType op pos t
  -- boolean operators
  | op `elem` [OpAnd, OpOr] = expectTypes [boolT] (InvalidBinaryType pos op) t
  -- arithmetic operators
  | op `elem` [OpPlus, OpMinus, OpMul, OpDiv] = expectTypes [boolT] (InvalidBinaryType pos op) t
  -- relational operators
  | op `elem` [OpEq, OpNeq, OpLess, OpLessEq, OpGreater, OpGreaterEq] =
    expectTypes [boolT, intT] (InvalidBinaryType pos op) t
  -- anything else (i.e., nothing)
  | otherwise = return ()

getPos :: Expr -> SourcePos
getPos (ConstInt pos _) = pos
getPos (ConstStr pos _) = pos
getPos (ConstBool pos _) = pos
getPos (UnOpExpr pos _ _) = pos
getPos (BinOpExpr pos _ _ _) = pos
getPos (LVal pos _) = pos
