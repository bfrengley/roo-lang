module SymbolTable where

import AST
import Control.Monad.State
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec (SourcePos, sourceColumn, sourceLine, sourceName)

class HasIdent a where
  getIdent :: a -> Ident

type TypeAliasNS = Map String TypeAlias

type FieldNS = Map String FieldDecl

type ProcNS = Map String ProcHead

type VarNS = Map String LocalSymbol

data TypeAlias
  = ArrayT Ident ArrayType
  | RecordT Ident FieldNS
  deriving (Show, Eq)

data LocalType
  = AliasT String
  | BuiltinT BuiltinType ProcParamPassMode
  | UnknownT
  deriving (Show, Eq)

data LocalSymbol
  = LocalSymbol Ident LocalType
  deriving (Show, Eq)

type SymbolState a = State [SymbolError] a

data SymbolTable = SymbolTable TypeAliasNS ProcNS VarNS

data SymbolError
  = Redefinition Ident Ident
  | Unknown Ident
  | InvalidArrayType Ident Ident
  | MissingMain
  deriving (Show)

getName :: Ident -> String
getName (Ident _ name) = name

printSymbolTableErrors :: String -> Program -> IO ()
printSymbolTableErrors source prog@(Program _ _ procs) =
  let (_, errs) = runState (buildGlobalSymbolTable prog >>= flip (foldM buildLocalSymbolTable) procs) []
      sourceLines = lines source
   in mapM_ (putStrLn . writeError sourceLines) $ reverse errs

buildLocalSymbolTable :: SymbolTable -> Procedure -> SymbolState SymbolTable
buildLocalSymbolTable
  (SymbolTable typeNS procNS _)
  (Procedure (ProcHead _ _ params) (ProcBody vars _)) =
    let makeVarNS =
          addSymbols (addParamSymbol typeNS) params >=> addSymbols (addLocalVarSymbols typeNS) vars
     in SymbolTable typeNS procNS <$> makeVarNS Map.empty

addParamSymbol :: TypeAliasNS -> VarNS -> ProcParam -> SymbolState VarNS
addParamSymbol typeNS varNS (ProcParam _ t ident) = do
  localT <- getParamType typeNS t
  insertSymbol varNS (LocalSymbol ident localT)

getParamType :: TypeAliasNS -> ProcParamType -> SymbolState LocalType
getParamType _ (ParamBuiltinT t pass) = return $ BuiltinT t pass
getParamType typeNS (ParamAliasT ident) = toAliasType typeNS ident

addLocalVarSymbols :: TypeAliasNS -> VarNS -> VarDecl -> SymbolState VarNS
addLocalVarSymbols typeNS varNS (VarDecl _ t idents) =
  do
    localT <- getLocalVarType typeNS t
    let insertVar ns ident = insertSymbol ns $ LocalSymbol ident localT
    foldM insertVar varNS idents

getLocalVarType :: TypeAliasNS -> VarType -> SymbolState LocalType
getLocalVarType _ (VarBuiltinT t) = return $ BuiltinT t PassByVal
getLocalVarType typeNS (VarAliasT ident) = toAliasType typeNS ident

toAliasType :: TypeAliasNS -> Ident -> SymbolState LocalType
toAliasType typeNS ident =
  let name = getName ident
   in if Map.member name typeNS
        then return $ AliasT name
        else addError (Unknown ident) >> return UnknownT

buildGlobalSymbolTable :: Program -> SymbolState SymbolTable
buildGlobalSymbolTable (Program recs arrs procs) = do
  aliasNS <- buildTypeAliasNS recs arrs
  procNS <- buildProcNS procs
  return $ SymbolTable aliasNS procNS Map.empty

buildProcNS :: [Procedure] -> SymbolState ProcNS
buildProcNS procs = do
  ns <- foldM addProc Map.empty procs
  when (Map.notMember "main" ns) $ addError MissingMain
  return ns

addSymbols :: (Foldable t, Monad m) => (b -> a -> m b) -> t a -> b -> m b
addSymbols addSymbol = flip (foldM addSymbol)

buildTypeAliasNS :: [RecordDef] -> [ArrayDef] -> SymbolState TypeAliasNS
buildTypeAliasNS recs arrs =
  addSymbols addRecordSymbol recs >=> addSymbols addArraySymbol arrs $ Map.empty

insertSymbol :: HasIdent b => Map String b -> b -> SymbolState (Map String b)
insertSymbol table sym =
  let ident = getIdent sym
      name = getName ident
   in case Map.lookup name table of
        Just prev -> addError (Redefinition ident (getIdent prev)) >> return table
        Nothing -> return $ Map.insert name sym table

addProc :: ProcNS -> Procedure -> SymbolState ProcNS
addProc table (Procedure head _) = insertSymbol table head

addArraySymbol :: TypeAliasNS -> ArrayDef -> SymbolState TypeAliasNS
addArraySymbol table (ArrayDef _ _ t ident) =
  let sym = ArrayT ident t
   in checkArrayType table t >> insertSymbol table sym

checkArrayType :: TypeAliasNS -> ArrayType -> SymbolState ()
checkArrayType table (ArrAliasT typeId) =
  let typename = getName typeId
   in case Map.lookup typename table of
        Nothing -> modify (Unknown typeId :)
        Just (ArrayT arrTypeId _) -> addError (InvalidArrayType typeId arrTypeId)
        _ -> return ()
checkArrayType _ _ = return ()

addRecordSymbol :: TypeAliasNS -> RecordDef -> SymbolState TypeAliasNS
addRecordSymbol table (RecordDef _ fields ident) =
  buildRecordNS fields >>= insertSymbol table . RecordT ident

buildRecordNS :: [FieldDecl] -> SymbolState FieldNS
buildRecordNS = foldM updateFieldNS Map.empty

updateFieldNS :: FieldNS -> FieldDecl -> SymbolState FieldNS
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

addError :: SymbolError -> SymbolState ()
addError err = modify (err :)

writeError :: [String] -> SymbolError -> String
writeError source (Redefinition (Ident pos name) (Ident pos' _)) =
  unlines
    [ writePos pos ++ ": error: `" ++ name ++ "` redefined",
      writeContext source pos,
      writePos pos' ++ ": note: previously defined here",
      writeContext source pos'
    ]
writeError source (Unknown (Ident pos name)) =
  unlines
    [ writePos pos ++ ": error: unknown type `" ++ name ++ "`",
      writeContext source pos
    ]
writeError source (InvalidArrayType (Ident pos name) (Ident pos' _)) =
  unlines
    [ writePos pos ++ ": error: invalid underlying type `" ++ name ++ "` for an array type",
      writeContext source pos,
      writePos pos' ++ ": note: type defined here",
      writeContext source pos'
    ]
writeError _ MissingMain =
  unlines ["error: no `main` procedure found"] -- position at end of source

writeContext :: [String] -> SourcePos -> String
writeContext source pos =
  source !! (sourceLine pos - 1) ++ "\n"
    ++ replicate (sourceColumn pos - 1) ' '
    ++ "^"

writePos :: SourcePos -> String
writePos pos = intercalate ":" [sourceName pos, show $ sourceLine pos, show $ sourceColumn pos]
