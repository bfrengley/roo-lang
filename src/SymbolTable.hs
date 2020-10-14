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

data SymbolTable = SymbolTable
  { aliases :: TypeAliasNS,
    procs :: ProcNS,
    env :: VarNS
  }

data SymbolError
  = Redefinition Ident Ident
  | Unknown Ident
  | InvalidArrayType Ident
  | MissingMain
  deriving (Show)

getName :: Ident -> String
getName (Ident _ name) = name

printSymbolTableErrors :: String -> Program -> IO ()
printSymbolTableErrors source prog =
  let (_, errs) = runState (buildGlobalSymbolTable prog) []
      sourceLines = lines source
   in mapM_ (putStrLn . writeError sourceLines) $ reverse errs

buildLocalSymbolTable :: Procedure -> SymbolTable -> SymbolState SymbolTable
buildLocalSymbolTable pro table = return table

-- addParam :: TypeAliasNS -> VarNS -> ProcParam -> SymbolState VarNS
-- addParam types ns param =

buildGlobalSymbolTable :: Program -> SymbolState SymbolTable
buildGlobalSymbolTable (Program recs arrs procs) = do
  aliasNS <- buildTypeAliasNS recs arrs
  procNS <- buildProcNS procs
  return $ SymbolTable {aliases = aliasNS, procs = procNS, env = Map.empty}

buildProcNS :: [Procedure] -> SymbolState ProcNS
buildProcNS procs = do
  ns <- foldM addProc Map.empty procs
  when (Map.notMember "main" ns) $ addError MissingMain
  return ns

buildTypeAliasNS :: [RecordDef] -> [ArrayDef] -> SymbolState TypeAliasNS
buildTypeAliasNS recs arrs =
  let addSymbols addSymbol defs = flip (foldM addSymbol) defs
   in addSymbols addRecordSymbol recs Map.empty >>= addSymbols addArraySymbol arrs

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
        Just (ArrayT _ _) -> addError (InvalidArrayType typeId)
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

addError :: SymbolError -> SymbolState ()
addError err = modify (err :)

writeError :: [String] -> SymbolError -> String
writeError source (Redefinition (Ident pos name) (Ident pos' _)) =
  unlines
    [ writePos pos ++ ": `" ++ name ++ "` redefined (previously defined at " ++ writePos pos' ++ ")",
      writeContext source pos
    ]
writeError source (Unknown (Ident pos name)) =
  unlines
    [ writePos pos ++ ": unknown type `" ++ name ++ "`",
      writeContext source pos
    ]
writeError source (InvalidArrayType (Ident pos name)) =
  unlines
    [ writePos pos ++ ": invalid underlying type `" ++ name ++ "` for an array type",
      writeContext source pos
    ]
writeError _ MissingMain =
  "no `main` procedure found" -- position at end of source

writeContext :: [String] -> SourcePos -> String
writeContext source pos =
  source !! (sourceLine pos - 1) ++ "\n"
    ++ replicate (sourceColumn pos - 1) ' '
    ++ "^"

writePos :: SourcePos -> String
writePos pos = intercalate ":" [sourceName pos, show $ sourceLine pos, show $ sourceColumn pos]
