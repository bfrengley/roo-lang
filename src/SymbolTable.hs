{-# LANGUAGE OverloadedStrings #-}

module SymbolTable where

import AST
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict (mapWriterT)
import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Semantics

class HasIdent a where
  getIdent :: a -> Ident

-- | Core type for symbolMap that have names/identifiers
data NamedSymbol = NamedSymbol Ident SymbolType deriving (Show, Eq)

-- | Symbol type for procedures, which each have a name then a list of parameters
--   (local variables are not stored in this type)
data ProcSymbol = ProcSymbol Ident [NamedSymbol] deriving (Show, Eq)

-- | Namespace of Procedures available in a program
type ProcNS = Map Text ProcSymbol

data FieldSymbol = FieldSymbol NamedSymbol Int deriving (Show, Eq)

-- | Namespace of fields within a record
type FieldNS = Map Text FieldSymbol

data TypeAlias
  = -- | Array types have a name identifier, a fixed size, and an underlying
    --   element type
    ArrayT Ident Int SymbolType
  | -- | Record types have a name identifier, and a namespace of fields
    RecordT Ident FieldNS
  deriving (Show, Eq)

-- | Namespace of Type Aliases available for use in a program
type TypeAliasNS = Map Text TypeAlias

data LocalSymbolType = ParamS | LocalVarS deriving (Show, Eq)

type StackSlot = Int

-- | Symbol type for parameters and local variables available within a procedure
--   (Includes what stack slot the symbol occupies in the procedure at runtime)
data LocalSymbol = LocalSymbol NamedSymbol LocalSymbolType StackSlot deriving (Show, Eq)

-- | Namespace of parameters and local variables that are in a procedure.
data LocalNS = LocalNS
  { -- | All local variables in a procedure AND all procedure parameters
    symbolMap :: Map Text LocalSymbol,
    -- | The (ordered) list of local symbols for a procedure
    -- Using this instead of just an OMap was necessitated by the last-minute discovery that the
    -- build server doesn't support `cabal update`, and `ordered-containers` is not found by
    -- default.
    -- It's less efficient, but we only have so much development time so compile time will suffer
    orderedSymbols :: [LocalSymbol]
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
lookupVar (SymbolTable _ _ locals) name = Map.lookup name (symbolMap locals)

lookupProcedure :: SymbolTable -> Text -> Maybe ProcSymbol
lookupProcedure (SymbolTable _ procs _) name = Map.lookup name procs

lookupType :: SymbolTable -> Text -> Maybe TypeAlias
lookupType (SymbolTable types _ _) name = Map.lookup name types

recordFields :: SymbolTable -> Text -> Maybe [FieldSymbol]
recordFields table typeName = do
  t <- lookupType table typeName
  case t of
    RecordT _ fieldNS -> Just $ toList fieldNS
    _ -> Nothing

lookupField :: SymbolTable -> Text -> Text -> Maybe FieldSymbol
lookupField table typeName fieldName = do
  t <- lookupType table typeName
  case t of
    RecordT _ fields -> Map.lookup fieldName fields
    _ -> Nothing

localVars :: SymbolTable -> [LocalSymbol]
localVars (SymbolTable _ _ locals) =
  filter (\(LocalSymbol _ symType _) -> symType == LocalVarS) $ orderedSymbols locals

localParams :: SymbolTable -> [LocalSymbol]
localParams (SymbolTable _ _ locals) =
  -- params are always first, so we can save a little bit of effort but giving up at the first
  -- local variable symbol by using `takeWhile` instead of `filter`
  takeWhile (\(LocalSymbol _ symType _) -> symType == ParamS) $ orderedSymbols locals

--
-- Symbol types
--

class TypedSymbol t where
  symbolType :: t -> SymbolType

instance TypedSymbol NamedSymbol where
  symbolType (NamedSymbol _ t) = t

instance TypedSymbol LocalSymbol where
  symbolType (LocalSymbol namedSym _ _) = symbolType namedSym

instance TypedSymbol FieldSymbol where
  symbolType (FieldSymbol namedSym _) = symbolType namedSym

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
      RecordT _ fields -> return $ Map.size fields
  typeSize _ UnknownT = Nothing
  typeSize _ _ = Just 1

instance SizedSymbol LocalSymbol where
  typeSize table (LocalSymbol sym _ _) = typeSize table sym

instance SizedSymbol NamedSymbol where
  typeSize table (NamedSymbol _ t) = typeSize table t

instance SizedSymbol ProcSymbol where
  typeSize table (ProcSymbol _ procParams) = sum <$> mapM (typeSize table) procParams

--
-- Symbol table generation
--

buildGlobalSymbolTable :: Program -> SemanticState () SymbolTable
buildGlobalSymbolTable (Program recs arrs procs) = do
  aliasNS <- buildTypeAliasNS recs arrs
  procNS <- buildProcNS procs
  return $ SymbolTable aliasNS procNS emptyLocalNS

emptyLocalNS :: LocalNS
emptyLocalNS = LocalNS {symbolMap = Map.empty, orderedSymbols = []}

buildLocalSymbolTable :: SymbolTable -> Procedure -> SemanticState () SymbolTable
buildLocalSymbolTable
  table@(SymbolTable typeNS procNS _)
  (Procedure (ProcHead _ _ procParams) (ProcBody vars _)) =
    let populateLocalNS =
          addSymbols (addParamSymbol table) procParams >=> addSymbols (addLocalVarSymbols table) vars
        -- this is some :galaxybrain: shit and honestly I'm still trying to grok it myself
        -- basically, we raise some function into the WriterT monad transformer to operate on the
        -- underlying monad (in this case, State)
        -- this lets us modify the underlying monad (in this case, we have a State StackSlot and
        -- we want just a State (), so we run the action and discard the slot state)
        localNS = mapWriterT (discardState 0) (populateLocalNS emptyLocalNS)
     in SymbolTable typeNS procNS <$> localNS

addParamSymbol :: SymbolTable -> LocalNS -> ProcParam -> SemanticState StackSlot LocalNS
addParamSymbol table localNS (ProcParam _ t ident) = do
  localT <- getParamType table t
  slot <- nextStackSlot table localT
  let sym = LocalSymbol (NamedSymbol ident localT) ParamS slot
  vars <- addSymbol (symbolMap localNS) sym
  return $ localNS {symbolMap = vars, orderedSymbols = orderedSymbols localNS ++ [sym]}

getParamType :: SymbolTable -> ProcParamType -> SemanticState StackSlot SymbolType
getParamType _ (ParamBuiltinT t pass) = return $ BuiltinT t pass
getParamType table (ParamAliasT ident) = toAliasType table ident PassByRef

addLocalVarSymbols :: SymbolTable -> LocalNS -> VarDecl -> SemanticState StackSlot LocalNS
addLocalVarSymbols table localNS (VarDecl _ t idents) = do
  localT <- getLocalVarType table t
  symbols <-
    mapM
      (\ident -> LocalSymbol (NamedSymbol ident localT) LocalVarS <$> nextStackSlot table localT)
      idents
  locals <- foldM addSymbol (symbolMap localNS) symbols
  return localNS {symbolMap = locals, orderedSymbols = orderedSymbols localNS ++ symbols}

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
   in if Map.member name typeNS
        then return $ AliasT name mode
        else addError (UnknownType ident) >> return UnknownT

buildProcNS :: [Procedure] -> SemanticState () ProcNS
buildProcNS procs = do
  ns <- foldM addProc Map.empty procs
  when (Map.notMember "main" ns) $ addError MissingMain
  return ns

addSymbols :: (Foldable t, Monad m) => (b -> a -> m b) -> t a -> b -> m b
addSymbols addSym = flip (foldM addSym)

buildTypeAliasNS :: [RecordDef] -> [ArrayDef] -> SemanticState () TypeAliasNS
buildTypeAliasNS recs arrs =
  addSymbols addRecordSymbol recs >=> addSymbols addArraySymbol arrs $ Map.empty

addSymbol :: HasIdent b => Map Text b -> b -> SemanticState a (Map Text b)
addSymbol table sym =
  let ident = getIdent sym
      name = getName ident
   in case Map.lookup name table of
        Just prev -> addError (Redefinition ident (getIdent prev)) >> return table
        Nothing -> return $ Map.insert name sym table

addProc :: ProcNS -> Procedure -> SemanticState () ProcNS
addProc table (Procedure (ProcHead _ ident procParams) _) =
  addSymbol table . ProcSymbol ident $ map symbolOfParam procParams

addArraySymbol :: TypeAliasNS -> ArrayDef -> SemanticState () TypeAliasNS
addArraySymbol table (ArrayDef _ size t ident) =
  let sym = ArrayT ident (fromInteger size) $ symbolTypeOfArrayType t
   in checkArrayType table t >> addSymbol table sym

checkArrayType :: TypeAliasNS -> ArrayType -> SemanticState () ()
checkArrayType table (ArrAliasT typeId) =
  let typename = getName typeId
   in case Map.lookup typename table of
        Nothing -> addError (UnknownType typeId)
        Just (ArrayT arrTypeId _ _) -> addError (InvalidArrayType typeId arrTypeId)
        _ -> return ()
checkArrayType _ _ = return ()

addRecordSymbol :: TypeAliasNS -> RecordDef -> SemanticState () TypeAliasNS
addRecordSymbol table (RecordDef _ fields ident) =
  buildRecordNS fields >>= addSymbol table . RecordT ident

buildRecordNS :: [FieldDecl] -> SemanticState () FieldNS
buildRecordNS decls =
  let nsState = foldM updateFieldNS Map.empty decls
   in mapWriterT (discardState 0) nsState

updateFieldNS :: FieldNS -> FieldDecl -> SemanticState Int FieldNS
updateFieldNS ns field@(FieldDecl _ _ ident) =
  let name = getName ident
   in case Map.lookup name ns of
        Just (FieldSymbol (NamedSymbol ident' _) _) -> addError (Redefinition ident ident') >> return ns
        Nothing -> do
          idx <- nextFieldIndex
          return $ Map.insert name (symbolOfField field idx) ns

nextFieldIndex :: SemanticState Int Int
nextFieldIndex = do
  idx <- get
  put $ idx + 1
  return idx

--
-- Conversion
--

symbolOfField :: FieldDecl -> Int -> FieldSymbol
symbolOfField (FieldDecl _ t ident) = FieldSymbol (NamedSymbol ident (BuiltinT t PassByVal))

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

-- We only need the stack slot state internally; when we return a symbol table it doesn't matter
-- any more. To discard it, we run the state computation (getting back the final value of the
-- action) and then re-raise it into a unit State monad.
--
-- This feels very dirty but if it works it works..?
discardState :: s -> State s a -> State () a
discardState initialState stateAction = return $ evalState stateAction initialState
