{-# LANGUAGE OverloadedStrings #-}

module SymbolTable where

import AST
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Semantics

class HasIdent a where
  getIdent :: a -> Ident

type TypeAliasNS = Map Text TypeAlias

type FieldNS = Map Text FieldDecl

type ProcNS = Map Text ProcHead

type VarNS = Map Text LocalSymbol

data TypeAlias
  = ArrayT Ident ArrayType
  | RecordT Ident FieldNS
  deriving (Show, Eq)

data LocalSymbol
  = LocalSymbol Ident LocalType
  deriving (Show, Eq)

data SymbolTable = SymbolTable TypeAliasNS ProcNS VarNS

lookupVar :: SymbolTable -> Text -> Maybe LocalSymbol
lookupVar (SymbolTable _ _ locals) = flip Map.lookup locals

lookupProcedure :: SymbolTable -> Text -> Maybe ProcHead
lookupProcedure (SymbolTable _ procs _) = flip Map.lookup procs

lookupType :: SymbolTable -> Text -> Maybe TypeAlias
lookupType (SymbolTable types _ _) = flip Map.lookup types

getName :: Ident -> Text
getName (Ident _ name) = T.pack name

buildLocalSymbolTable :: SymbolTable -> Procedure -> SemanticState SymbolTable
buildLocalSymbolTable
  (SymbolTable typeNS procNS _)
  (Procedure (ProcHead _ _ params) (ProcBody vars _)) =
    let makeVarNS =
          addSymbols (addParamSymbol typeNS) params >=> addSymbols (addLocalVarSymbols typeNS) vars
     in (SymbolTable typeNS procNS <$> makeVarNS Map.empty)

addParamSymbol :: TypeAliasNS -> VarNS -> ProcParam -> SemanticState VarNS
addParamSymbol typeNS varNS (ProcParam _ t ident) = do
  localT <- getParamType typeNS t
  insertSymbol varNS (LocalSymbol ident localT)

getParamType :: TypeAliasNS -> ProcParamType -> SemanticState LocalType
getParamType _ (ParamBuiltinT t pass) = return $ BuiltinT t pass
getParamType typeNS (ParamAliasT ident) = toAliasType typeNS ident PassByRef

addLocalVarSymbols :: TypeAliasNS -> VarNS -> VarDecl -> SemanticState VarNS
addLocalVarSymbols typeNS varNS (VarDecl _ t idents) =
  do
    localT <- getLocalVarType typeNS t
    let insertVar ns ident = insertSymbol ns $ LocalSymbol ident localT
    foldM insertVar varNS idents

getLocalVarType :: TypeAliasNS -> VarType -> SemanticState LocalType
getLocalVarType _ (VarBuiltinT t) = return $ BuiltinT t PassByVal
getLocalVarType typeNS (VarAliasT ident) = toAliasType typeNS ident PassByVal

toAliasType :: TypeAliasNS -> Ident -> ProcParamPassMode -> SemanticState LocalType
toAliasType typeNS ident mode =
  let name = getName ident
   in if Map.member name typeNS
        then return $ AliasT name mode
        else addError (UnknownType ident) >> return UnknownT

buildGlobalSymbolTable :: Program -> SemanticState SymbolTable
buildGlobalSymbolTable (Program recs arrs procs) = do
  aliasNS <- buildTypeAliasNS recs arrs
  procNS <- buildProcNS procs
  return $ SymbolTable aliasNS procNS Map.empty

buildProcNS :: [Procedure] -> SemanticState ProcNS
buildProcNS procs = do
  ns <- foldM addProc Map.empty procs
  when (Map.notMember "main" ns) $ addError MissingMain
  return ns

addSymbols :: (Foldable t, Monad m) => (b -> a -> m b) -> t a -> b -> m b
addSymbols addSymbol = flip (foldM addSymbol)

buildTypeAliasNS :: [RecordDef] -> [ArrayDef] -> SemanticState TypeAliasNS
buildTypeAliasNS recs arrs =
  addSymbols addRecordSymbol recs >=> addSymbols addArraySymbol arrs $ Map.empty

insertSymbol :: HasIdent b => Map Text b -> b -> SemanticState (Map Text b)
insertSymbol table sym =
  let ident = getIdent sym
      name = getName ident
   in case Map.lookup name table of
        Just prev -> addError (Redefinition ident (getIdent prev)) >> return table
        Nothing -> return $ Map.insert name sym table

addProc :: ProcNS -> Procedure -> SemanticState ProcNS
addProc table (Procedure head _) = insertSymbol table head

addArraySymbol :: TypeAliasNS -> ArrayDef -> SemanticState TypeAliasNS
addArraySymbol table (ArrayDef _ _ t ident) =
  let sym = ArrayT ident t
   in checkArrayType table t >> insertSymbol table sym

checkArrayType :: TypeAliasNS -> ArrayType -> SemanticState ()
checkArrayType table (ArrAliasT typeId) =
  let typename = getName typeId
   in case Map.lookup typename table of
        Nothing -> modify (UnknownType typeId :)
        Just (ArrayT arrTypeId _) -> addError (InvalidArrayType typeId arrTypeId)
        _ -> return ()
checkArrayType _ _ = return ()

addRecordSymbol :: TypeAliasNS -> RecordDef -> SemanticState TypeAliasNS
addRecordSymbol table (RecordDef _ fields ident) =
  buildRecordNS fields >>= insertSymbol table . RecordT ident

buildRecordNS :: [FieldDecl] -> SemanticState FieldNS
buildRecordNS = foldM updateFieldNS Map.empty

updateFieldNS :: FieldNS -> FieldDecl -> SemanticState FieldNS
updateFieldNS ns field@(FieldDecl _ _ ident) =
  let name = getName ident
   in case Map.lookup name ns of
        Just (FieldDecl _ _ ident') -> addError (Redefinition ident ident') >> return ns
        Nothing -> return $ Map.insert name field ns

instance HasIdent TypeAlias where
  getIdent (RecordT ident _) = ident
  getIdent (ArrayT ident _) = ident

instance HasIdent ProcHead where
  getIdent (ProcHead _ ident _) = ident

instance HasIdent LocalSymbol where
  getIdent (LocalSymbol ident _) = ident
