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
generateProcedureCode (Roo.Procedure (Roo.ProcHead sourcePos (Roo.Ident idSourcePos ident) args) (Roo.ProcBody varDecls statements)) symbolTable =
  Oz.LabelledBlock
    (Oz.Label ident)
    (
      concatMap
      (
        \x ->
          let
            printNotYetImplemented = \stmntDescription ->
              naiveWriteLnStringConst (show (stmntDescription ++ " not yet implemented"))
          in
            case x of
              Roo.SAtom sPos (Roo.Assign lvalue expr) ->
                printNotYetImplemented "RooAST.Assign"
              Roo.SAtom sPos (Roo.Read lvalue) ->
                printNotYetImplemented "RooAST.Read"
              Roo.SAtom sPos (Roo.Write expr) ->
                (
                  case expr of
                    Roo.ConstBool sp bool -> naiveWriteBoolConst bool
                    Roo.ConstInt sp int -> naiveWriteIntegerConst int
                    Roo.ConstStr sp str -> naiveWriteStringConst str
                    Roo.LVal sp lvalue ->
                      -- I think symbol table is required here
                      printNotYetImplemented "Write for LVal"
                    Roo.BinOpExpr sp binop expr1 expr2 ->
                      -- Need to intermediately compute the expression result
                      printNotYetImplemented "Write for BinOpExpr"
                    Roo.UnOpExpr sp unop expr ->
                      -- Need to intermediately compute the expression result
                      printNotYetImplemented "Write for UnOpExpr"
                )
              Roo.SAtom sPos (Roo.WriteLn expr) ->
                (
                  case expr of
                    Roo.ConstBool sp bool -> naiveWriteLnBoolConst bool
                    Roo.ConstInt sp int -> naiveWriteLnIntegerConst int
                    Roo.ConstStr sp str -> naiveWriteLnStringConst str
                    Roo.LVal sp lvalue ->
                      -- I think symbol table is required here
                      printNotYetImplemented "WriteLn for LVal"
                    Roo.BinOpExpr sp binop expr1 expr2 ->
                      -- Need to intermediately compute the expression result
                      printNotYetImplemented "WriteLn for BinOpExpr"
                    Roo.UnOpExpr sp unop expr ->
                      -- Need to intermediately compute the expression result
                      printNotYetImplemented "WriteLn for UnOpExpr"
                )
              Roo.SAtom sPos (Roo.Call ident exprs) ->
                printNotYetImplemented "RooAST.Call"
              Roo.SAtom sPos atomicStatement ->
                printNotYetImplemented "RooAST.SAtom"
              Roo.SComp sPos compStatement ->
                printNotYetImplemented "RooAST.SComp"

      )
      statements
  )

naiveWriteBoolConst :: Bool -> [Oz.Instruction]
naiveWriteBoolConst val =
  [
    Oz.InstrIntConst
      (Oz.Register 0)
      (Oz.IntegerConst (
          -- I think 1 is how true vals get represented...
          if val then 1 else 0
        )
      ),
    Oz.InstrCallBuiltin Oz.BuiltinPrintBool
  ]

naiveWriteIntegerConst :: Integer -> [Oz.Instruction]
naiveWriteIntegerConst val =
  [
    Oz.InstrIntConst (Oz.Register 0) (Oz.IntegerConst val),
    Oz.InstrCallBuiltin Oz.BuiltinPrintInt
  ]


naiveWriteStringConst :: String -> [Oz.Instruction]
naiveWriteStringConst val =
  [
    Oz.InstrStringConst (Oz.Register 0) (Oz.StringConst (show val)),
    Oz.InstrCallBuiltin Oz.BuiltinPrintString
  ]

naiveWriteLnBoolConst :: Bool -> [Oz.Instruction]
naiveWriteLnBoolConst val =
  concat [
    naiveWriteBoolConst val,
    naiveWriteStringConst "\n"
  ]
naiveWriteLnIntegerConst :: Integer -> [Oz.Instruction]
naiveWriteLnIntegerConst val =
  concat [
    naiveWriteIntegerConst val,
    naiveWriteStringConst "\n"
  ]

naiveWriteLnStringConst :: String -> [Oz.Instruction]
naiveWriteLnStringConst val =
  concat [
    naiveWriteStringConst val,
    naiveWriteStringConst "\n"
  ]