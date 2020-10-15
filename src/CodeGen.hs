module CodeGen where

import AST
import Control.Monad.State (runState)
import qualified OzAST as Oz
import Semantics (SemanticError)
import SymbolTable (SymbolTable, buildGlobalSymbolTable, buildLocalSymbolTable)

programEntry :: [Oz.LabelledBlock] -> Oz.Program
programEntry = Oz.Program [Oz.InstrCall (Oz.Label "proc_main"), Oz.InstrHalt]

generateCode :: Program -> Either [SemanticError] Oz.Program
generateCode prog@(Program _ _ procs) =
  let tables =
        ( do
            globalTable <- buildGlobalSymbolTable prog
            mapM (buildLocalSymbolTable globalTable) procs
        )
   in case runState tables [] of
        (tables, []) -> Right $ programEntry $ zipWith genProcCode tables procs
        (_, errs) -> Left errs

genProcCode :: SymbolTable -> Procedure -> Oz.LabelledBlock
genProcCode _ _ = Oz.LabelledBlock (Oz.Label "") []
