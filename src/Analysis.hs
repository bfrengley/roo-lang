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
import Util ((=%=))

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

isAliasT :: SymbolType -> Bool
isAliasT (AliasT _ _) = True
isAliasT _ = False

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

operatorPos :: Expr -> SourcePos
operatorPos (ConstInt pos _) = pos
operatorPos (ConstStr pos _) = pos
operatorPos (ConstBool pos _) = pos
operatorPos (UnOpExpr pos _ _) = pos
operatorPos (LVal pos _) = pos
operatorPos (BinOpExpr pos _ _ _) = pos
