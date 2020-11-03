{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Analysis
-- Description: This poorly named module contains a few of the basics for analysis, but most of it
--              is actually in CodeGen, since we do it inline.
-- Maintainer: Stewart Webb <sjwebb@student.unimelb.edu.au>
--             Ben Frengley <bfrengley@student.unimelb.edu.au>
--
-- This module defines some basic functions used for analysis. Since the rest of the analysis
-- happens inline in CodeGen, this is a bit empty, but what can you do
module Analysis where

import AST
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Semantics
import SymbolTable
import Text.Parsec (SourcePos)
import Util ((=%=))

-- | Get the type of an expression. The type is actually constant based on the first level of the
-- expression, so we use this to avoid recursion.
getExprType :: SymbolTable -> Expr -> SymbolType
getExprType _ (ConstInt _ _) = intT
getExprType _ (ConstBool _ _) = boolT
getExprType _ (ConstStr _ _) = StringT
getExprType _ (UnOpExpr _ op _) = unOpReturnType op
getExprType _ (BinOpExpr _ op _ _) = binOpReturnType op
getExprType table (LVal _ lval) = getLvalType table lval

-- | Get the type of an lvalue, returning the bottom type of things are weird (it's unknown, it's
-- a non-array type with an index expression, or it's a non-record type with a field access).
getLvalType :: SymbolTable -> LValue -> SymbolType
getLvalType table (LValue _ ident index field) =
  let varType = symbolType <$> lookupVar table (getName ident)
      indexedType = varType >>= narrowArrayType table index
      fieldType = indexedType >>= narrowFieldType table field
   in fromMaybe UnknownT fieldType

-- | Try to narrow a symbol type based on the index expression.
narrowArrayType :: SymbolTable -> Maybe Expr -> SymbolType -> Maybe SymbolType
-- if we have no index expression, then we don't need to narrow it
narrowArrayType _ Nothing baseT = Just baseT
-- if we have an index expression, it's only valid where the underlying type is an alias type
narrowArrayType table (Just _) (AliasT typeName mode) = do
  innerT <- lookupType table typeName
  -- only aliases of an array type make sense to have an index expression
  case innerT of
    ArrayT _ _ t -> Just $ setMode mode t
    _ -> Nothing
narrowArrayType _ _ _ = Nothing

-- | Try to narrow a symbol type based on the field expression.
narrowFieldType :: SymbolTable -> Maybe Ident -> SymbolType -> Maybe SymbolType
narrowFieldType _ Nothing baseT = Just baseT
narrowFieldType table (Just field) (AliasT typeName mode) =
  setMode mode . symbolType <$> lookupField table typeName (getName field)
narrowFieldType _ _ _ = Nothing

-- | Override the mode of a symbol.
setMode :: ProcParamPassMode -> SymbolType -> SymbolType
setMode mode (AliasT name _) = AliasT name mode
setMode mode (BuiltinT t _) = BuiltinT t mode
setMode _ t = t

--
-- Convenience aliases of the primitive types, since they're a bit cumbersome otherwise.
--

boolT :: SymbolType
boolT = BuiltinT TBool PassByVal

intT :: SymbolType
intT = BuiltinT TInt PassByVal

isAliasT :: SymbolType -> Bool
isAliasT (AliasT _ _) = True
isAliasT _ = False

--
-- Convenience methods for checking if types match and recorded errors if they don't.
-- These probably make more sense in context.
--

-- | Check if a type matches the expected type, recording an error if not. The error is supplied
-- partially applied with the type of error and the rest of its context.
expectType ::
  SymbolType ->
  (SymbolType -> SymbolType -> SemanticError) ->
  SymbolType ->
  SemanticState a ()
expectType expected makeErr actual =
  unless (actual =%= expected) (addError $ makeErr actual expected)

-- | Check if a type matches any of the expected types, recording an error if not. The error is
-- supplied partially applied with the type of error and the rest of its context.
expectTypes ::
  [SymbolType] ->
  (SymbolType -> [SymbolType] -> SemanticError) ->
  SymbolType ->
  SemanticState a ()
expectTypes expected makeErr actual =
  unless (any (actual =%=) expected) (addError $ makeErr actual expected)

-- | Check that the type matches the expected type for the unary operator supplied.
expectUnOpType :: UnaryOp -> SourcePos -> SymbolType -> SemanticState a ()
expectUnOpType OpNot pos = expectType boolT (InvalidUnaryType pos OpNot)
expectUnOpType OpNeg pos = expectType intT (InvalidUnaryType pos OpNeg)

unOpReturnType :: UnaryOp -> SymbolType
unOpReturnType OpNot = boolT
unOpReturnType OpNeg = intT

-- | Check that the type matches the expected type for the binary operator supplied.
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
  | otherwise = UnknownT -- this is never the case

isRelOp :: BinaryOp -> Bool
isRelOp = (`elem` [OpEq, OpNeq, OpLess, OpLessEq, OpGreater, OpGreaterEq])

isBooleanOp :: BinaryOp -> Bool
isBooleanOp = (`elem` [OpAnd, OpOr])

isArithOp :: BinaryOp -> Bool
isArithOp = (`elem` [OpPlus, OpMinus, OpMul, OpDiv])

-- | Get the position of the operator in an expression, for nicer error messages.
operatorPos :: Expr -> SourcePos
operatorPos (ConstInt pos _) = pos
operatorPos (ConstStr pos _) = pos
operatorPos (ConstBool pos _) = pos
operatorPos (UnOpExpr pos _ _) = pos
operatorPos (LVal pos _) = pos
operatorPos (BinOpExpr pos _ _ _) = pos
