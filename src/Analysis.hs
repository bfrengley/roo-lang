{-# LANGUAGE OverloadedStrings #-}

module Analysis where

import AST
import Control.Monad
import Control.Monad.State.Strict (evalState)
import Control.Monad.Trans.Writer.Strict (runWriterT)
import Data.Maybe (fromMaybe)
import Semantics
import SymbolTable
import Text.Parsec (SourcePos)
import Util ((=%=), (=>>))

-- Perform semantic analysis of the program and return the Symbol Tables for each procedure if there are no errors
analyseProgram :: Program -> Either [SemanticError] [SymbolTable]
analyseProgram prog@(Program _ _ procs) =
  let genTables = do
        globalTable <- buildGlobalSymbolTable prog
        localTables <- mapM (buildLocalSymbolTable globalTable) procs
        -- analyse every procedure to record any errors
        -- zipWithM_ analyseProcedure localTables procs
        return localTables
   in case evalState (runWriterT genTables) () of
        (tables, []) -> Right tables
        (_, errs) -> Left $ reverse errs

-- analyseProcedure :: SymbolTable -> Procedure -> SemanticState a ()
-- analyseProcedure table (Procedure _ (ProcBody _ stmts)) = mapM_ (analyseStmt table) stmts

-- analyseStmt :: SymbolTable -> Stmt -> SemanticState a ()
-- analyseStmt table (SAtom pos stmt) = analyseAtomicStmt table pos stmt
-- analyseStmt table (SComp pos stmt) = analyseCompStmt table pos stmt

-- analyseCompStmt :: SymbolTable -> SourcePos -> CompositeStmt -> SemanticState a ()
-- analyseCompStmt _ _ _ = return ()

-- analyseAtomicStmt :: SymbolTable -> SourcePos -> AtomicStmt -> SemanticState a ()
-- analyseAtomicStmt table pos (Assign lval@(LValue _ ident _ _) expr) = do
--   lvalT <- evaluateLvalType table lval
--   evaluategetExprType table expr >>= expectType lvalT (InvalidAssign pos ident)
-- analyseAtomicStmt _ _ _ = return ()

validateExpr :: SymbolTable -> Expr -> SemanticState a ()
validateExpr table (UnOpExpr _ op expr) =
  expectUnOpType op (getPos expr) $ getExprType table expr
validateExpr table (BinOpExpr pos op left right) =
  let leftT = getExprType table left
      rightT = getExprType table right
   in do
        expectBinOpType op (getPos left) leftT
        expectBinOpType op (getPos right) rightT
        unless (leftT =%= rightT) $ addError $ BinaryTypeMismatch pos op leftT rightT
validateExpr table (LVal _ (LValue pos ident index field)) = do
  return ()

getExprType :: SymbolTable -> Expr -> SymbolType
getExprType _ (ConstInt _ _) = intT
getExprType _ (ConstBool _ _) = boolT
getExprType _ (ConstStr _ _) = StringT
getExprType _ (UnOpExpr _ op _) = unOpReturnType op
getExprType _ (BinOpExpr _ op _ _) = binOpReturnType op
getExprType table (LVal _ lval) = getLvalType table lval

getLvalType :: SymbolTable -> LValue -> SymbolType
getLvalType table (LValue _ ident index field) =
  let varType = symbolType <$> lookupVar table (getName ident)
      indexedType = varType >>= narrowArrayType table index
      fieldType = indexedType >>= narrowFieldType table field
   in fromMaybe UnknownT fieldType

narrowArrayType :: SymbolTable -> Maybe Expr -> SymbolType -> Maybe SymbolType
-- if we have no index expression, then we don't need to narrow it
narrowArrayType _ Nothing baseT = Just baseT
-- if we have an index expression, it's only valid where the underlying type is an alias type
narrowArrayType table (Just _) (AliasT typeName _) = do
  innerT <- lookupType table typeName
  -- only aliases of an array type make sense to have an index expression
  case innerT of
    ArrayT _ _ t -> Just t
    _ -> Nothing
narrowArrayType _ _ _ = Nothing

narrowFieldType :: SymbolTable -> Maybe Ident -> SymbolType -> Maybe SymbolType
narrowFieldType _ Nothing baseT = Just baseT
narrowFieldType table (Just field) (AliasT typeName _) =
  symbolType <$> lookupField table typeName (getName field)
narrowFieldType _ _ _ = Nothing

boolT :: SymbolType
boolT = BuiltinT TBool PassByVal

intT :: SymbolType
intT = BuiltinT TInt PassByVal

expectType ::
  SymbolType ->
  (SymbolType -> SymbolType -> SemanticError) ->
  SymbolType ->
  SemanticState a ()
expectType expected makeErr actual =
  unless (actual =%= expected) (addError $ makeErr actual expected)

expectTypes ::
  [SymbolType] ->
  (SymbolType -> [SymbolType] -> SemanticError) ->
  SymbolType ->
  SemanticState a ()
expectTypes expected makeErr actual =
  unless (any (actual =%=) expected) (addError $ makeErr actual expected)

expectUnOpType :: UnaryOp -> SourcePos -> SymbolType -> SemanticState a ()
expectUnOpType OpNot pos = expectType boolT (InvalidUnaryType pos OpNot)
expectUnOpType OpNeg pos = expectType intT (InvalidUnaryType pos OpNeg)

unOpReturnType :: UnaryOp -> SymbolType
unOpReturnType OpNot = boolT
unOpReturnType OpNeg = intT

expectBinOpType :: BinaryOp -> SourcePos -> SymbolType -> SemanticState a ()
expectBinOpType op pos t
  | isBooleanOp op = expectTypes [boolT] (InvalidBinaryType pos op) t
  | isRelOp op = expectTypes [boolT, intT] (InvalidBinaryType pos op) t
  | isArithOp op = expectTypes [intT] (InvalidBinaryType pos op) t
  -- anything else (i.e., never)
  | otherwise = return ()

binOpReturnType :: BinaryOp -> SymbolType
binOpReturnType op
  | isRelOp op = boolT
  | isBooleanOp op = boolT
  | isArithOp op = intT
  | otherwise = UnknownT

isRelOp :: BinaryOp -> Bool
isRelOp = (`elem` [OpEq, OpNeq, OpLess, OpLessEq, OpGreater, OpGreaterEq])

isBooleanOp :: BinaryOp -> Bool
isBooleanOp = (`elem` [OpAnd, OpOr])

isArithOp :: BinaryOp -> Bool
isArithOp = (`elem` [OpPlus, OpMinus, OpMul, OpDiv])

getPos :: Expr -> SourcePos
getPos (ConstInt pos _) = pos
getPos (ConstStr pos _) = pos
getPos (ConstBool pos _) = pos
getPos (UnOpExpr pos _ _) = pos
getPos (BinOpExpr pos _ _ _) = pos
getPos (LVal pos _) = pos
