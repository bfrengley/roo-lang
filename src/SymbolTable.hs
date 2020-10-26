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

data NamedSymbol = NamedSymbol Ident SymbolType deriving (Show, Eq)

type FieldNS = Map Text NamedSymbol

data ProcSymbol = ProcSymbol Ident [NamedSymbol] deriving (Show, Eq)

type ProcNS = Map Text ProcSymbol

data TypeAlias
  = ArrayT Ident Int SymbolType
  | RecordT Ident FieldNS
  deriving (Show, Eq)

type TypeAliasNS = Map Text TypeAlias

type VarNS = Map Text NamedSymbol

data SymbolTable = SymbolTable TypeAliasNS ProcNS VarNS

--
-- Lookup
--

lookupVar :: SymbolTable -> Text -> Maybe NamedSymbol
lookupVar (SymbolTable _ _ locals) = flip Map.lookup locals

lookupProcedure :: SymbolTable -> Text -> Maybe ProcSymbol
lookupProcedure (SymbolTable _ procs _) name = Map.lookup name procs

lookupType :: SymbolTable -> Text -> Maybe TypeAlias
lookupType (SymbolTable types _ _) = flip Map.lookup types

--
-- Symbol sizes
--

class SizedSymbol t where
  typeSize :: SymbolTable -> t -> Maybe Int

instance SizedSymbol SymbolType where
  typeSize table (AliasT name PassByVal) = do
    alias <- lookupType table name
    case alias of
      ArrayT _ length t -> do
        underlyingSize <- typeSize table t
        return $ length * underlyingSize
      RecordT _ fields -> return $ Map.size fields
  typeSize _ UnknownT = Nothing
  typeSize _ _ = Just 1

instance SizedSymbol NamedSymbol where
  typeSize table (NamedSymbol _ t) = typeSize table t

instance SizedSymbol ProcSymbol where
  typeSize table (ProcSymbol _ params) = sum <$> mapM (typeSize table) params

--
-- Symbol table generation
--

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
  addSymbol varNS (NamedSymbol ident localT)

getParamType :: TypeAliasNS -> ProcParamType -> SemanticState SymbolType
getParamType _ (ParamBuiltinT t pass) = return $ BuiltinT t pass
getParamType typeNS (ParamAliasT ident) = toAliasType typeNS ident PassByRef

addLocalVarSymbols :: TypeAliasNS -> VarNS -> VarDecl -> SemanticState VarNS
addLocalVarSymbols typeNS varNS (VarDecl _ t idents) =
  do
    localT <- getLocalVarType typeNS t
    let insertVar ns ident = addSymbol ns $ NamedSymbol ident localT
    foldM insertVar varNS idents

getLocalVarType :: TypeAliasNS -> VarType -> SemanticState SymbolType
getLocalVarType _ (VarBuiltinT t) = return $ BuiltinT t PassByVal
getLocalVarType typeNS (VarAliasT ident) = toAliasType typeNS ident PassByVal

toAliasType :: TypeAliasNS -> Ident -> ProcParamPassMode -> SemanticState SymbolType
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

addSymbol :: HasIdent b => Map Text b -> b -> SemanticState (Map Text b)
addSymbol table sym =
  let ident = getIdent sym
      name = getName ident
   in case Map.lookup name table of
        Just prev -> addError (Redefinition ident (getIdent prev)) >> return table
        Nothing -> return $ Map.insert name sym table

addProc :: ProcNS -> Procedure -> SemanticState ProcNS
addProc table (Procedure (ProcHead _ ident params) _) =
  addSymbol table . ProcSymbol ident $ map symbolOfParam params

addArraySymbol :: TypeAliasNS -> ArrayDef -> SemanticState TypeAliasNS
addArraySymbol table (ArrayDef _ size t ident) =
  let sym = ArrayT ident (fromInteger size) $ symbolTypeOfArrayType t
   in checkArrayType table t >> addSymbol table sym

checkArrayType :: TypeAliasNS -> ArrayType -> SemanticState ()
checkArrayType table (ArrAliasT typeId) =
  let typename = getName typeId
   in case Map.lookup typename table of
        Nothing -> modify (UnknownType typeId :)
        Just (ArrayT arrTypeId _ _) -> addError (InvalidArrayType typeId arrTypeId)
        _ -> return ()
checkArrayType _ _ = return ()

addRecordSymbol :: TypeAliasNS -> RecordDef -> SemanticState TypeAliasNS
addRecordSymbol table (RecordDef _ fields ident) =
  buildRecordNS fields >>= addSymbol table . RecordT ident

buildRecordNS :: [FieldDecl] -> SemanticState FieldNS
buildRecordNS = foldM updateFieldNS Map.empty

updateFieldNS :: FieldNS -> FieldDecl -> SemanticState FieldNS
updateFieldNS ns field@(FieldDecl _ _ ident) =
  let name = getName ident
   in case Map.lookup name ns of
        Just (NamedSymbol ident' _) -> addError (Redefinition ident ident') >> return ns
        Nothing -> return $ Map.insert name (symbolOfField field) ns

--
-- Conversion
--

symbolOfField :: FieldDecl -> NamedSymbol
symbolOfField (FieldDecl _ t ident) = NamedSymbol ident (BuiltinT t PassByVal)

symbolTypeOfArrayType :: ArrayType -> SymbolType
symbolTypeOfArrayType (ArrBuiltinT t) = BuiltinT t PassByVal
symbolTypeOfArrayType (ArrAliasT ident) = AliasT (getName ident) PassByVal

symbolOfParam :: ProcParam -> NamedSymbol
symbolOfParam (ProcParam _ (ParamAliasT typeId) ident) =
  NamedSymbol ident $ AliasT (getName typeId) PassByRef
symbolOfParam (ProcParam _ (ParamBuiltinT t mode) ident) =
  NamedSymbol ident $ BuiltinT t mode

instance HasIdent TypeAlias where
  getIdent (RecordT ident _) = ident
  getIdent (ArrayT ident _ _) = ident

instance HasIdent ProcSymbol where
  getIdent (ProcSymbol ident _) = ident

instance HasIdent NamedSymbol where
  getIdent (NamedSymbol ident _) = ident
