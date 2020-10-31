{-# LANGUAGE OverloadedStrings #-}

module SymbolTable where

import AST
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict (mapWriterT)
import Data.Foldable (Foldable (toList))
import Data.Map.Ordered (OMap, (|<))
import qualified Data.Map.Ordered as OMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Semantics

class HasIdent a where
  getIdent :: a -> Ident

-- | Core type for symbols that have names/identifiers
data NamedSymbol = NamedSymbol Ident SymbolType deriving (Show, Eq)

-- | Symbol type for procedures, which each have a name then a list of parameters
--   (local variables are not stored in this type)
data ProcSymbol = ProcSymbol Ident [NamedSymbol] deriving (Show, Eq)

-- | Namespace of Procedures available in a program
type ProcNS = OMap Text ProcSymbol

-- | Namespace of fields within a record
type FieldNS = OMap Text NamedSymbol

data TypeAlias
  = -- | Array types have a name identifier, a fixed size, and an underlying
    --   element type
    ArrayT Ident Int SymbolType
  | -- | Record types have a name identifier, and a namespace of fields
    RecordT Ident FieldNS
  deriving (Show, Eq)

-- | Namespace of Type Aliases available for use in a program
type TypeAliasNS = OMap Text TypeAlias

data LocalSymbolType = ParamS | LocalVarS deriving (Show, Eq)

type StackSlot = Int

-- | Symbol type for parameters and local variables available within a procedure
--   (Includes what stack slot the symbol occupies in the procedure at runtime)
data LocalSymbol = LocalSymbol NamedSymbol LocalSymbolType StackSlot deriving (Show, Eq)

-- | Namespace of parameters and local variables that are in a procedure.
data LocalNS = LocalNS
  { -- | All local variables in a procedure AND all procedure parameters
    symbols :: OMap Text LocalSymbol,
    -- | The (ordered) list of paramaters for a procedure
    params :: [LocalSymbol]
  }
  deriving (Show, Eq)

data SymbolTable = SymbolTable TypeAliasNS ProcNS LocalNS deriving (Show, Eq)

--
-- Lookup
--

getName :: Ident -> Text
getName (Ident _ name) = T.pack name

getStackSlot :: LocalSymbol -> StackSlot
getStackSlot (LocalSymbol _ _ slot) = slot

lookupVar :: SymbolTable -> Text -> Maybe LocalSymbol
lookupVar (SymbolTable _ _ locals) name = OMap.lookup name (symbols locals)

lookupProcedure :: SymbolTable -> Text -> Maybe ProcSymbol
lookupProcedure (SymbolTable _ procs _) name = OMap.lookup name procs

lookupType :: SymbolTable -> Text -> Maybe TypeAlias
lookupType (SymbolTable types _ _) name = OMap.lookup name types

recordFields :: SymbolTable -> Text -> Maybe [NamedSymbol]
recordFields table typeName = do
  t <- lookupType table typeName
  case t of
    RecordT _ fieldNS -> Just $ toList fieldNS
    _ -> Nothing

lookupField :: SymbolTable -> Text -> Text -> Maybe NamedSymbol
lookupField table typeName fieldName = do
  t <- lookupType table typeName
  case t of
    RecordT _ fields -> OMap.lookup fieldName fields
    _ -> Nothing

localVars :: SymbolTable -> [LocalSymbol]
localVars (SymbolTable _ _ locals) =
  let syms = toList $ symbols locals
   in filter (\(LocalSymbol _ symType _) -> symType == LocalVarS) syms

localParams :: SymbolTable -> [LocalSymbol]
localParams (SymbolTable _ _ locals) =
  let syms = toList $ symbols locals
   in filter (\(LocalSymbol _ symType _) -> symType == ParamS) syms

--
-- Symbol types
--

class TypedSymbol t where
  symbolType :: t -> SymbolType

instance TypedSymbol NamedSymbol where
  symbolType (NamedSymbol _ t) = t

instance TypedSymbol LocalSymbol where
  symbolType (LocalSymbol namedSym _ _) = symbolType namedSym

--
-- Symbol sizes
--

class SizedSymbol t where
  typeSize :: SymbolTable -> t -> Maybe Int

instance SizedSymbol SymbolType where
  typeSize table (AliasT name PassByVal) = do
    alias <- lookupType table name
    case alias of
      ArrayT _ len t -> do
        underlyingSize <- typeSize table t
        return $ len * underlyingSize
      RecordT _ fields -> return $ OMap.size fields
  typeSize _ UnknownT = Nothing
  typeSize _ _ = Just 1

instance SizedSymbol LocalSymbol where
  typeSize table (LocalSymbol sym _ _) = typeSize table sym

instance SizedSymbol NamedSymbol where
  typeSize table (NamedSymbol _ t) = typeSize table t

instance SizedSymbol ProcSymbol where
  typeSize table (ProcSymbol _ params) = sum <$> mapM (typeSize table) params

--
-- Symbol table generation
--

emptyLocalNS :: LocalNS
emptyLocalNS = LocalNS {symbols = OMap.empty, params = []}

buildLocalSymbolTable :: SymbolTable -> Procedure -> SemanticState () SymbolTable
buildLocalSymbolTable
  table@(SymbolTable typeNS procNS _)
  (Procedure (ProcHead _ _ params) (ProcBody vars _)) =
    let populateLocalNS =
          addSymbols (addParamSymbol table) params >=> addSymbols (addLocalVarSymbols table) vars
        -- this is some :galaxybrain: shit and honestly I'm still trying to grok it myself
        -- basically, we raise some function into the WriterT monad transformer to operate on the
        -- underlying monad (in this case, State)
        -- this lets us modify the underlying monad (in this case, we have a State StackSlot and
        -- we want just a State (), so we run the action and discard the slot state)
        localNS = mapWriterT discardSlotState (populateLocalNS emptyLocalNS)
     in SymbolTable typeNS procNS <$> localNS

-- We only need the stack slot state internally; when we return a symbol table it doesn't matter
-- any more. To discard it, we run the state computation (getting back the final value of the
-- action) and then re-raise it into a unit State monad.
--
-- This feels very dirty but if it works it works..?
discardSlotState :: State StackSlot a -> State () a
discardSlotState s1 = return $ evalState s1 0

addParamSymbol :: SymbolTable -> LocalNS -> ProcParam -> SemanticState StackSlot LocalNS
addParamSymbol table localNS (ProcParam _ t ident) = do
  localT <- getParamType table t
  slot <- nextStackSlot table localT
  let sym = LocalSymbol (NamedSymbol ident localT) ParamS slot
  vars <- addSymbol (symbols localNS) sym
  return $ localNS {symbols = vars, params = params localNS ++ [sym]}

getParamType :: SymbolTable -> ProcParamType -> SemanticState StackSlot SymbolType
getParamType _ (ParamBuiltinT t pass) = return $ BuiltinT t pass
getParamType table (ParamAliasT ident) = toAliasType table ident PassByRef

addLocalVarSymbols :: SymbolTable -> LocalNS -> VarDecl -> SemanticState StackSlot LocalNS
addLocalVarSymbols table localNS (VarDecl _ t idents) = do
  localT <- getLocalVarType table t
  let insertVar ns ident =
        nextStackSlot table localT
          >>= addSymbol ns . LocalSymbol (NamedSymbol ident localT) LocalVarS
  locals <- foldM insertVar (symbols localNS) idents
  return localNS {symbols = locals}

getLocalVarType :: SymbolTable -> VarType -> SemanticState StackSlot SymbolType
getLocalVarType _ (VarBuiltinT t) = return $ BuiltinT t PassByVal
getLocalVarType table (VarAliasT ident) = toAliasType table ident PassByVal

nextStackSlot :: SymbolTable -> SymbolType -> SemanticState StackSlot StackSlot
nextStackSlot table t = do
  slot <- get
  put $ slot + fromMaybe 0 (typeSize table t)
  return slot

toAliasType :: SymbolTable -> Ident -> ProcParamPassMode -> SemanticState StackSlot SymbolType
toAliasType (SymbolTable typeNS _ _) ident mode =
  let name = getName ident
   in if OMap.member name typeNS
        then return $ AliasT name mode
        else addError (UnknownType ident) >> return UnknownT

buildGlobalSymbolTable :: Program -> SemanticState () SymbolTable
buildGlobalSymbolTable (Program recs arrs procs) = do
  aliasNS <- buildTypeAliasNS recs arrs
  procNS <- buildProcNS procs
  return $ SymbolTable aliasNS procNS emptyLocalNS

buildProcNS :: [Procedure] -> SemanticState () ProcNS
buildProcNS procs = do
  ns <- foldM addProc OMap.empty procs
  when (OMap.notMember "main" ns) $ addError MissingMain
  return ns

addSymbols :: (Foldable t, Monad m) => (b -> a -> m b) -> t a -> b -> m b
addSymbols addSym = flip (foldM addSym)

buildTypeAliasNS :: [RecordDef] -> [ArrayDef] -> SemanticState () TypeAliasNS
buildTypeAliasNS recs arrs =
  addSymbols addRecordSymbol recs >=> addSymbols addArraySymbol arrs $ OMap.empty

addSymbol :: HasIdent b => OMap Text b -> b -> SemanticState a (OMap Text b)
addSymbol table sym =
  let ident = getIdent sym
      name = getName ident
   in case OMap.lookup name table of
        Just prev -> addError (Redefinition ident (getIdent prev)) >> return table
        Nothing -> return $ (name, sym) |< table

addProc :: ProcNS -> Procedure -> SemanticState () ProcNS
addProc table (Procedure (ProcHead _ ident params) _) =
  addSymbol table . ProcSymbol ident $ map symbolOfParam params

addArraySymbol :: TypeAliasNS -> ArrayDef -> SemanticState () TypeAliasNS
addArraySymbol table (ArrayDef _ size t ident) =
  let sym = ArrayT ident (fromInteger size) $ symbolTypeOfArrayType t
   in checkArrayType table t >> addSymbol table sym

checkArrayType :: TypeAliasNS -> ArrayType -> SemanticState () ()
checkArrayType table (ArrAliasT typeId) =
  let typename = getName typeId
   in case OMap.lookup typename table of
        Nothing -> addError (UnknownType typeId)
        Just (ArrayT arrTypeId _ _) -> addError (InvalidArrayType typeId arrTypeId)
        _ -> return ()
checkArrayType _ _ = return ()

addRecordSymbol :: TypeAliasNS -> RecordDef -> SemanticState () TypeAliasNS
addRecordSymbol table (RecordDef _ fields ident) =
  buildRecordNS fields >>= addSymbol table . RecordT ident

buildRecordNS :: [FieldDecl] -> SemanticState () FieldNS
buildRecordNS = foldM updateFieldNS OMap.empty

updateFieldNS :: FieldNS -> FieldDecl -> SemanticState () FieldNS
updateFieldNS ns field@(FieldDecl _ _ ident) =
  let name = getName ident
   in case OMap.lookup name ns of
        Just (NamedSymbol ident' _) -> addError (Redefinition ident ident') >> return ns
        Nothing -> return $ (name, symbolOfField field) |< ns

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

instance HasIdent LocalSymbol where
  getIdent (LocalSymbol sym _ _) = getIdent sym
