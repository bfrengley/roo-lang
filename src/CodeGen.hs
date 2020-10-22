module CodeGen where

import AST
import Analyse
import Control.Monad (zipWithM_)
import Control.Monad.State (runState)
import qualified OzAST as Oz
import Semantics (SemanticError)
import SymbolTable (SymbolTable, buildGlobalSymbolTable, buildLocalSymbolTable)

programEntry :: [Oz.LabelledBlock] -> Oz.Program
programEntry = Oz.Program [Oz.InstrCall (Oz.Label "proc_main"), Oz.InstrHalt]

generateCode :: Program -> Either [SemanticError] Oz.Program
generateCode prog@(Program _ _ procs) =
  let genTables =
        ( do
            globalTable <- buildGlobalSymbolTable prog
            localTables <- mapM (buildLocalSymbolTable globalTable) procs
            -- analyse every procedure to record any errors
            zipWithM_ analyseProcedure localTables procs
            return localTables
        )
   in case runState genTables [] of
        -- no errors; generate code
        (tables, []) -> Right $ programEntry $ zipWith genProcCode tables procs
        -- if we have errors, don't bother generating any code
        (_, errs) -> Left $ reverse errs

genProcCode :: SymbolTable -> Procedure -> Oz.LabelledBlock
genProcCode _ _ = Oz.LabelledBlock (Oz.Label "") []
