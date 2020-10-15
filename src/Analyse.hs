module Analyse where

import AST
import Semantics
import SymbolTable (SymbolTable)

analyseProcedure :: SymbolTable -> Procedure -> SemanticState ()
analyseProcedure _ _ = return ()
