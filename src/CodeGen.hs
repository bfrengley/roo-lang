module CodeGen where

import Control.Monad.State.Strict
import Data.Map (size)
import qualified Data.Text as T

import qualified AST as Roo
import qualified OzAST as Oz
import SymbolTable

data OzIOT = OzIOInt | OzIOBool | OzIOStr

-- track the lowest free register
type RegisterState a = State Oz.Register a

runtimeBoilerplate :: [Oz.ProgramLine]
runtimeBoilerplate =
  map Oz.InstructionLine
    [ Oz.InstrCall (Oz.Label "main"),
      Oz.InstrHalt
    ]

generateCode :: Roo.Program -> [SymbolTable] -> Oz.Program
generateCode (Roo.Program _ _ procs) procedureSymbolTables =
  Oz.Program
    (runtimeBoilerplate ++ concat (zipWith generateProcedureCode procs procedureSymbolTables))

generateProcedureCode :: Roo.Procedure -> SymbolTable -> [Oz.ProgramLine]
generateProcedureCode procedure@(Roo.Procedure (Roo.ProcHead _ (Roo.Ident _ ident) args) (Roo.ProcBody varDecls statements)) symbolTable =
  concat [
    [Oz.LabelLine (Oz.Label ident)],
    stackSetup,
    instrs,
    stackCleanup,
    [Oz.InstructionLine Oz.InstrReturn]
  ]
  where
    stackSize = calculateStackSize procedure symbolTable
    instrs = (concat $ evalState (mapM (generateStmtCode symbolTable) statements) initialRegister)
    stackSetup =
      if (stackSize > 0) then
        let
          saveArgs = zipWith (\i arg -> Oz.InstructionLine $ generateArgumentStackSaveInstr (Oz.Register i) 0) [0..] args
        in
          [Oz.InstructionLine $ Oz.InstrPushStackFrame (Oz.Framesize stackSize)] ++ saveArgs
      else
        []
    stackCleanup =
      if (stackSize > 0) then
        [Oz.InstructionLine $ Oz.InstrPopStackFrame (Oz.Framesize stackSize)]
      else
        []

calculateStackSize :: Roo.Procedure -> SymbolTable -> Int
calculateStackSize (Roo.Procedure (Roo.ProcHead _ (Roo.Ident _ _) args) (Roo.ProcBody varDecls _)) symbolTable =
  sum (argSizes ++ varSizes)
  where
    argSizes = map (\arg -> calculateParamStackSize arg symbolTable) args
    varSizes = map (\decl -> calculateLocalVarDeclStackSize decl symbolTable) varDecls

calculateLocalVarDeclStackSize :: Roo.VarDecl -> SymbolTable -> Int
calculateLocalVarDeclStackSize (Roo.VarDecl _ (Roo.VarBuiltinT _) varIdents) _ =
  -- Builtin types are always of size 1
  -- VarDecl could be declaring multiple variables so multiply by the number of identifiers
  1 * length varIdents
calculateLocalVarDeclStackSize (Roo.VarDecl varDeclSp (Roo.VarAliasT (Roo.Ident typeAliasIdentSp typeAliasIdent)) varIdents) symbolTable = 
  let numberOfVars = length varIdents in
  -- This variable declaration is of an aliased type. It will either be a record or an array.
  -- Hit up the Symbol table for info about the type alias
  case (lookupType symbolTable (T.pack typeAliasIdent)) of
    Just (RecordT _ rt) ->
      let numberOfRecordFields = size rt in
      -- Records can only have boolean or integer types for fields... so the
      -- stack size is just the number of fields
      numberOfRecordFields * 1
    Just (ArrayT _ (Roo.ArrBuiltinT a)) ->
      -- Built-in types are always size 1, so just multiply by the array size
      -- and the # of array declarations
      1 * numberOfVars -- XXX: need to multiply by array size!!!
    Just (ArrayT _ (Roo.ArrAliasT (Roo.Ident _ arrayElemTypeAliasIdent))) ->
      -- The type of the array elements is a type alias. Arrays are only allowed to contain
      -- elements of built-in type or record elements, so the type alias *should* just be a record
      case (lookupType symbolTable (T.pack arrayElemTypeAliasIdent)) of
        Just (RecordT _ rt) ->
          let numberOfRecordFields = size rt in
          -- Again, record size is just # of fields, so just multiply by array
          -- size and # of array declarations
          numberOfRecordFields * numberOfVars -- XXX: need to multiply by array size!!!
        Just _ -> error $ "Type alias " ++ arrayElemTypeAliasIdent ++ " shouldn't be allowed to be used as an array element type?"
        Nothing -> error $ "Type alias " ++ arrayElemTypeAliasIdent ++ " (for array type " ++ (show typeAliasIdent) ++ " at " ++ (show typeAliasIdentSp) ++ ") could not be found in symbol table for calculating stack size?"
    Nothing -> error $ "Type alias " ++ typeAliasIdent ++ " (at " ++ (show varDeclSp) ++ ") could not be found in symbol table for calculating stack size?"

calculateParamStackSize :: Roo.ProcParam -> SymbolTable -> Int
calculateParamStackSize (Roo.ProcParam _ (Roo.ParamBuiltinT _ passMode) (Roo.Ident _ ident)) _ =
  case passMode of
    Roo.PassByRef ->
      -- Size is 1 because we only need to store an address, which only takes
      -- up one stack slot
      1
    Roo.PassByVal ->
      -- arrays and records can't be passed by value in Roo, so it can be
      -- assumed that any parameter passed by value is a builtin type that
      -- only needs one stack slot for storage
      1
calculateParamStackSize (Roo.ProcParam _ (Roo.ParamAliasT typeIdent) (Roo.Ident paramIdentSp paramIdent)) symbolTable =
  -- Values of type-aliased type must be passed by reference...
  -- so all that's needed is one slot for the address
  1

generateArgumentStackSaveInstr :: Oz.Register -> Int -> Oz.Instruction
generateArgumentStackSaveInstr register@(Oz.Register registerNumber) stackOffset =
  let stackSlot = stackOffset + registerNumber in
  Oz.InstrStore (Oz.StackSlot stackSlot) register

generateStmtCode :: SymbolTable -> Roo.Stmt -> RegisterState [Oz.ProgramLine]
generateStmtCode symbolTable (Roo.SAtom _ (Roo.Write expr)) =
  case expr of
    Roo.ConstBool _ bool -> do return $ map Oz.InstructionLine (naiveWriteBoolConst bool)
    Roo.ConstInt _ int -> do return $ map Oz.InstructionLine (naiveWriteIntegerConst int)
    Roo.ConstStr _ str -> do return $ map Oz.InstructionLine (naiveWriteStringConst str)
    expr@(Roo.BinOpExpr _ _ arg1 arg2) -> do
      (exprInstrs, exprReg) <- generateExpEvalCode expr symbolTable
      let writeExprOutputInstrs = writeValFromRegister exprReg (ioTypeFromExpr expr symbolTable)
      return $ map Oz.InstructionLine (exprInstrs ++ writeExprOutputInstrs)
    expr@(Roo.UnOpExpr _ _ arg) -> do
      (exprInstrs, exprReg) <- generateExpEvalCode expr symbolTable
      let writeExprOutputInstrs = writeValFromRegister exprReg (ioTypeFromExpr expr symbolTable)
      return $ map Oz.InstructionLine (exprInstrs ++ writeExprOutputInstrs)
    otherExpr -> return $ map Oz.InstructionLine (printNotYetImplemented $ "Write for expression type " ++ (show otherExpr))

generateStmtCode symbolTable (Roo.SAtom _ (Roo.WriteLn expr)) =
  case expr of
    Roo.ConstBool _ bool -> do return $ map Oz.InstructionLine (naiveWriteLnBoolConst bool)
    Roo.ConstInt _ int -> do return $ map Oz.InstructionLine (naiveWriteLnIntegerConst int)
    Roo.ConstStr _ str -> do return $ map Oz.InstructionLine (naiveWriteLnStringConst str)
    expr@(Roo.BinOpExpr _ _ arg1 arg2) -> do
      (exprInstrs, exprReg) <- generateExpEvalCode expr symbolTable
      let writeExprOutputInstrs = writeLnValFromRegister exprReg (ioTypeFromExpr expr symbolTable)
      return $ map Oz.InstructionLine (exprInstrs ++ writeExprOutputInstrs)
    expr@(Roo.UnOpExpr _ _ arg) -> do
      (exprInstrs, exprReg) <- generateExpEvalCode expr symbolTable
      let writeExprOutputInstrs = writeLnValFromRegister exprReg (ioTypeFromExpr expr symbolTable)
      return $ map Oz.InstructionLine (exprInstrs ++ writeExprOutputInstrs)
    otherExpr -> return $ map Oz.InstructionLine (printNotYetImplemented $ "WriteLn for expression type " ++ (show otherExpr))

generateStmtCode _ stmt = do return $ map Oz.InstructionLine (printNotYetImplemented $ "Statement type " ++ (show stmt))

-- | Determine what Oz IO type an expression result is (for writeln etc.)
ioTypeFromExpr :: Roo.Expr -> SymbolTable -> OzIOT
ioTypeFromExpr (Roo.ConstBool _ _) _ = OzIOBool
ioTypeFromExpr (Roo.ConstInt _ _) _ = OzIOInt
ioTypeFromExpr (Roo.ConstStr _ _) _ = OzIOStr
ioTypeFromExpr (Roo.BinOpExpr _ op arg1 arg2) _ = case op of
  -- I believe that this is correct and that it's not necessary to inspect
  -- the operands any further to determine an output type
  Roo.OpPlus -> OzIOInt
  Roo.OpMinus -> OzIOInt
  Roo.OpMul -> OzIOInt
  Roo.OpDiv -> OzIOInt
  Roo.OpAnd -> OzIOBool
  Roo.OpOr -> OzIOBool
  Roo.OpEq -> OzIOBool
  Roo.OpNeq -> OzIOBool
  Roo.OpGreater -> OzIOBool
  Roo.OpGreaterEq -> OzIOBool
  Roo.OpLess -> OzIOBool
  Roo.OpLessEq -> OzIOBool
ioTypeFromExpr (Roo.UnOpExpr _ op arg) _ = case op of
  Roo.OpNot -> OzIOBool
  Roo.OpNeg -> OzIOInt
ioTypeFromExpr (Roo.LVal _ ident) symTable = error "LValue type lookup not yet implemented"

-- | Generate code for evaluating an expression. The result of calculating the
--   expresison will end up in the returned register
generateExpEvalCode :: Roo.Expr -> SymbolTable -> RegisterState ([Oz.Instruction], Oz.Register)
generateExpEvalCode (Roo.ConstBool _ val) symTable = do
  reg <- getNextRegister
  return ([Oz.InstrIntConst reg $ Oz.boolConst val ], reg)
generateExpEvalCode (Roo.ConstInt _ val) symTable = do
  reg <- getNextRegister
  return ([Oz.InstrIntConst reg $ Oz.IntegerConst val ], reg)
generateExpEvalCode (Roo.ConstStr _ str) symTable = do
  reg <- getNextRegister
  return ([Oz.InstrStringConst reg $ Oz.StringConst str], reg)
generateExpEvalCode (Roo.UnOpExpr _ op expr) symTable = do
  (sourceInstrs, sourceReg) <- generateExpEvalCode expr symTable
  destReg <- getNextRegister
  return (sourceInstrs ++ [unOpInstruction op destReg sourceReg], destReg)
generateExpEvalCode (Roo.BinOpExpr _ op left right) symTable = do
  (leftInstrs, leftOperandReg) <- generateExpEvalCode left symTable
  (rightInstrs, rightOperandReg) <- generateExpEvalCode right symTable
  destRegister <- getNextRegister
  return (leftInstrs ++ rightInstrs ++ [binOpInstruction op destRegister leftOperandReg rightOperandReg], destRegister)
-- evaluateExpression (Roo.LVal _)

printNotYetImplemented :: String -> [Oz.Instruction]
printNotYetImplemented stmntDescription = naiveWriteLnStringConst (show (stmntDescription ++ " not yet implemented"))

naiveWriteBoolConst :: Bool -> [Oz.Instruction]
naiveWriteBoolConst val =
  [ Oz.InstrIntConst
      (Oz.Register 0)
      (Oz.boolConst val),
    Oz.InstrCallBuiltin Oz.BuiltinPrintBool
  ]

naiveWriteIntegerConst :: Integer -> [Oz.Instruction]
naiveWriteIntegerConst val =
  [ Oz.InstrIntConst (Oz.Register 0) (Oz.IntegerConst val),
    Oz.InstrCallBuiltin Oz.BuiltinPrintInt
  ]

naiveWriteStringConst :: String -> [Oz.Instruction]
naiveWriteStringConst val =
  [ Oz.InstrStringConst (Oz.Register 0) (Oz.StringConst (show val)),
    Oz.InstrCallBuiltin Oz.BuiltinPrintString
  ]

naiveWriteNewlineConst :: [Oz.Instruction]
naiveWriteNewlineConst = naiveWriteStringConst "\n"

naiveWriteLnBoolConst :: Bool -> [Oz.Instruction]
naiveWriteLnBoolConst val =
  concat
    [ naiveWriteBoolConst val,
      naiveWriteNewlineConst
    ]

naiveWriteLnIntegerConst :: Integer -> [Oz.Instruction]
naiveWriteLnIntegerConst val =
  concat
    [ naiveWriteIntegerConst val,
      naiveWriteNewlineConst
    ]

naiveWriteLnStringConst :: String -> [Oz.Instruction]
naiveWriteLnStringConst val =
  concat
    [ naiveWriteStringConst val,
      naiveWriteNewlineConst
    ]

writeValFromRegister :: Oz.Register -> OzIOT -> [Oz.Instruction]
writeValFromRegister reg typ =
  [
    Oz.InstrMove (Oz.Register 0) reg,
    case typ of
      OzIOBool -> Oz.InstrCallBuiltin Oz.BuiltinPrintBool
      OzIOInt -> Oz.InstrCallBuiltin Oz.BuiltinPrintInt
      OzIOStr -> Oz.InstrCallBuiltin Oz.BuiltinPrintString
  ]
writeLnValFromRegister :: Oz.Register -> OzIOT -> [Oz.Instruction]
writeLnValFromRegister reg typ =
  concat
    [
      writeValFromRegister reg typ,
      naiveWriteNewlineConst
    ]

incrRegister :: Oz.Register -> Oz.Register
incrRegister (Oz.Register r) = Oz.Register $ r + 1

unOpInstruction :: Roo.UnaryOp -> Oz.Register -> Oz.Register -> Oz.Instruction
unOpInstruction Roo.OpNot = Oz.InstrNot
unOpInstruction Roo.OpNeg = Oz.InstrNegInt

binOpInstruction :: Roo.BinaryOp -> Oz.Register -> Oz.Register -> Oz.Register -> Oz.Instruction
binOpInstruction Roo.OpPlus = Oz.InstrAddInt
binOpInstruction Roo.OpMinus = Oz.InstrSubInt
binOpInstruction Roo.OpMul = Oz.InstrMulInt
binOpInstruction Roo.OpDiv = Oz.InstrDivInt
binOpInstruction Roo.OpAnd = Oz.InstrAnd
binOpInstruction Roo.OpOr = Oz.InstrOr
binOpInstruction Roo.OpEq = Oz.InstrCmpEqualInt
binOpInstruction Roo.OpNeq = Oz.InstrCmpNotEqualInt
binOpInstruction Roo.OpGreater = Oz.InstrCmpGreaterThanInt
binOpInstruction Roo.OpGreaterEq = Oz.InstrCmpGreaterEqualInt
binOpInstruction Roo.OpLess = Oz.InstrCmpLessThanInt
binOpInstruction Roo.OpLessEq = Oz.InstrCmpLessEqualInt

getNextRegister :: RegisterState Oz.Register
getNextRegister = do
  reg <- get
  put $ incrRegister reg
  return reg

initialRegister :: Oz.Register
initialRegister = Oz.Register 1
