module SymbolTable where

import AST
import Control.Monad.State
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec (SourcePos, sourceColumn, sourceLine, sourceName)

type TypeAliasNS = Map String Ident

type FieldNS = Map String Ident

type ProcNS = Map String Ident

type VarNS = Map String Ident

data GlobalSymbol
  = ArrayS Ident ArrayType
  | RecordS Ident FieldNS
  | ProcS ProcHead
  deriving (Show, Eq)

type GlobalSymbolTable = Map String GlobalSymbol

data LocalType
  = AliasT String
  | BuiltinT BuiltinType ProcParamPassMode
  deriving (Show, Eq)

data LocalSymbol
  = LocalSymbol SourcePos Ident LocalType
  deriving (Show, Eq)

type SymbolState a = State [SymbolError] a

type LocalSymbolTable = Map String LocalSymbol

data SymbolTable = SymbolTable
  { global :: GlobalSymbolTable,
    env :: LocalSymbolTable
  }

data SymbolError
  = Redefinition Ident Ident
  | Unknown Ident
  | InvalidArrayType Ident
  deriving (Show)

getName :: Ident -> String
getName (Ident _ name) = name

printSymbolTableErrors :: String -> Program -> IO ()
printSymbolTableErrors source prog =
  let (_, errs) = buildGlobalSymbols prog
      sourceLines = lines source
   in mapM_ (putStrLn . writeError sourceLines) $ reverse errs

buildGlobalSymbols :: Program -> (GlobalSymbolTable, [SymbolError])
buildGlobalSymbols (Program recs arrs procs) =
  let addSymbols addSymbol defs = flip (foldM addSymbol) defs
      populateTable =
        addSymbols addRecordSymbol recs
          >=> addSymbols addArraySymbol arrs
          >=> addSymbols addProcSymbol procs
   in runState (populateTable Map.empty) []

insertGlobalSymbol :: GlobalSymbolTable -> GlobalSymbol -> SymbolState GlobalSymbolTable
insertGlobalSymbol table sym =
  let ident = getIdent sym
      name = getName ident
   in case Map.lookup name table of
        Just prev -> addError (Redefinition ident (getIdent prev)) >> return table
        Nothing -> return $ Map.insert name sym table

addProcSymbol :: GlobalSymbolTable -> Procedure -> SymbolState GlobalSymbolTable
addProcSymbol table (Procedure head _) = insertGlobalSymbol table $ ProcS head

addArraySymbol :: GlobalSymbolTable -> ArrayDef -> SymbolState GlobalSymbolTable
addArraySymbol table (ArrayDef _ _ t ident) =
  let sym = ArrayS ident t
   in checkArrayType table t >> insertGlobalSymbol table sym

checkArrayType :: GlobalSymbolTable -> ArrayType -> SymbolState ()
checkArrayType table (ArrAliasT typeId) =
  let typename = getName typeId
   in case Map.lookup typename table of
        Nothing -> modify (Unknown typeId :)
        Just (ArrayS _ _) -> addError (InvalidArrayType typeId)
        _ -> return ()
checkArrayType _ _ = return ()

addRecordSymbol :: GlobalSymbolTable -> RecordDef -> SymbolState GlobalSymbolTable
addRecordSymbol table (RecordDef _ fields ident) =
  buildRecordNS fields >>= insertGlobalSymbol table . RecordS ident

buildRecordNS :: [FieldDecl] -> SymbolState FieldNS
buildRecordNS = foldM updateFieldNS Map.empty

updateFieldNS :: FieldNS -> FieldDecl -> SymbolState FieldNS
updateFieldNS ns (FieldDecl _ _ ident) =
  let name = getName ident
   in case Map.lookup name ns of
        Just ident' -> addError (Redefinition ident ident') >> return ns
        Nothing -> return $ Map.insert name ident ns

getIdent :: GlobalSymbol -> Ident
getIdent (RecordS ident _) = ident
getIdent (ArrayS ident _) = ident
getIdent (ProcS (ProcHead _ ident _)) = ident

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
    [ writePos pos ++ ": cannot declare array of unknown type `" ++ name ++ "`",
      writeContext source pos
    ]
writeError source (InvalidArrayType (Ident pos name)) =
  unlines
    [ writePos pos ++ ": invalid underlying type `" ++ name ++ "` for an array type",
      writeContext source pos
    ]

writeContext :: [String] -> SourcePos -> String
writeContext source pos =
  source !! (sourceLine pos - 1) ++ "\n"
    ++ replicate (sourceColumn pos - 1) ' '
    ++ "^"

writePos :: SourcePos -> String
writePos pos = intercalate ":" [sourceName pos, show $ sourceLine pos, show $ sourceColumn pos]
