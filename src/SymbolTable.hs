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

type SymbolState a = State [InvalidSymbol] a

type LocalSymbolTable = Map String LocalSymbol

data SymbolTable = SymbolTable
  { global :: GlobalSymbolTable,
    env :: LocalSymbolTable
  }

data InvalidSymbol
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

buildGlobalSymbols :: Program -> (GlobalSymbolTable, [InvalidSymbol])
buildGlobalSymbols (Program recs arrs procs) =
  let table =
        foldM buildRecordSymbol Map.empty recs
          >>= flip (foldM buildArraySymbol) arrs
          >>= flip (foldM buildProcSymbol) procs
   in runState table []

insertGlobalSymbol :: GlobalSymbolTable -> GlobalSymbol -> SymbolState GlobalSymbolTable
insertGlobalSymbol table sym =
  let ident = getIdent sym
      name = getName ident
   in case Map.lookup name table of
        Just prev -> modify (Redefinition ident (getIdent prev) :) >> return table
        Nothing -> return $ Map.insert name sym table

buildProcSymbol :: GlobalSymbolTable -> Procedure -> SymbolState GlobalSymbolTable
buildProcSymbol table (Procedure head _) = insertGlobalSymbol table $ ProcS head

buildArraySymbol :: GlobalSymbolTable -> ArrayDef -> SymbolState GlobalSymbolTable
buildArraySymbol table (ArrayDef _ _ t ident) =
  let sym = ArrayS ident t
   in checkArrayType table t >> insertGlobalSymbol table sym

checkArrayType :: GlobalSymbolTable -> ArrayType -> SymbolState ()
checkArrayType table (ArrAliasT typeId) =
  let typename = getName typeId
   in case Map.lookup typename table of
        Nothing -> modify (Unknown typeId :)
        Just (ArrayS _ _) -> modify (InvalidArrayType typeId :)
        _ -> return ()
checkArrayType _ _ = return ()

buildRecordSymbol :: GlobalSymbolTable -> RecordDef -> SymbolState GlobalSymbolTable
buildRecordSymbol table (RecordDef _ fields ident) =
  buildRecordNS fields >>= insertGlobalSymbol table . RecordS ident

buildRecordNS :: [FieldDecl] -> SymbolState FieldNS
buildRecordNS = foldM updateFieldNS Map.empty

updateFieldNS :: FieldNS -> FieldDecl -> SymbolState FieldNS
updateFieldNS ns (FieldDecl _ _ ident) =
  let name = getName ident
   in case Map.lookup name ns of
        Just ident' -> modify (Redefinition ident ident' :) >> return ns
        Nothing -> return $ Map.insert name ident ns

getIdent :: GlobalSymbol -> Ident
getIdent (RecordS ident _) = ident
getIdent (ArrayS ident _) = ident
getIdent (ProcS (ProcHead _ ident _)) = ident

writeError :: [String] -> InvalidSymbol -> String
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
