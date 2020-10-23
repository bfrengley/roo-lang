module CodeGen where

import Control.Monad (zipWithM_)
import Control.Monad.State (runState)

import qualified AST as Roo
import qualified OzAST as Oz
import SymbolTable (SymbolTable)

runtimeBoilerplate =
  [
    Oz.InstrCall (Oz.Label "main"),
    Oz.InstrHalt
  ]

generateCode :: Roo.Program -> [SymbolTable] -> Oz.Program
generateCode (Roo.Program _ _ procs) procedureSymbolTables =
  Oz.Program
    runtimeBoilerplate
    (zipWith generateProcedureCode procs procedureSymbolTables)

generateProcedureCode :: Roo.Procedure -> SymbolTable -> Oz.LabelledBlock
generateProcedureCode _ _ = Oz.LabelledBlock (Oz.Label "") []
