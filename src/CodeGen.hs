module CodeGen where

import qualified AST as Roo
import Control.Monad.State.Strict
import qualified Data.Map as Map
-- import Data.Map.Ordered
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified OzAST as Oz
import Semantics (SymbolType (..))
import SymbolTable

data OzIOT = OzIOInt | OzIOBool | OzIOStr

-- track the lowest free register
type RegisterState a = State Oz.Register a

runtimeBoilerplate :: [Oz.ProgramLine]
runtimeBoilerplate =
  map
    Oz.InstructionLine
    [ Oz.InstrCall (Oz.Label "main"),
      Oz.InstrHalt
    ]

generateCode :: Roo.Program -> [SymbolTable] -> Oz.Program
generateCode (Roo.Program _ _ procs) tables =
  Oz.Program
    (runtimeBoilerplate ++ concat (zipWith generateProcedureCode tables procs))

generateProcedureCode :: SymbolTable -> Roo.Procedure -> [Oz.ProgramLine]
generateProcedureCode symbolTable procedure@(Roo.Procedure (Roo.ProcHead _ (Roo.Ident _ procName) args) (Roo.ProcBody varDecls statements)) =
  concat
    [ [Oz.LabelLine (Oz.Label procName)],
      stackSetup,
      instrs,
      stackCleanup,
      [Oz.InstructionLine Oz.InstrReturn]
    ]
  where
    stackSize = case calculateStackSize symbolTable of
      Just size -> size
      Nothing -> error $ "Stack size calculation for procedure " ++ procName ++ " failed"
    instrs = concat $ evalState (mapM (generateStmtCode symbolTable) statements) initialRegister
    stackSetup =
      if stackSize > 0
        then
          let saveArgs = zipWith (\i arg -> Oz.InstructionLine $ generateArgumentStackSaveInstr (Oz.Register i) 0) [0 ..] args
              initVars = generateVariableInitializeCodeForProc symbolTable
           in concat
                [ [Oz.InstructionLine (Oz.InstrPushStackFrame $ Oz.Framesize stackSize)],
                  saveArgs,
                  initVars
                ]
        else []
    stackCleanup =
      if stackSize > 0
        then [Oz.InstructionLine $ Oz.InstrPopStackFrame (Oz.Framesize stackSize)]
        else []

calculateStackSize :: SymbolTable -> Maybe Int
calculateStackSize table@(SymbolTable _ _ vars) = do
  paramSizes <- mapM (typeSize table) $ params vars
  varSizes <- mapM (typeSize table) $ localVars table
  return $ sum paramSizes + sum varSizes

generateArgumentStackSaveInstr :: Oz.Register -> Int -> Oz.Instruction
generateArgumentStackSaveInstr register@(Oz.Register registerNumber) stackOffset =
  let stackSlotNo = stackOffset + registerNumber
   in Oz.InstrStore (Oz.StackSlot stackSlotNo) register

stackSize :: SizedSymbol s => s -> SymbolTable -> Int
stackSize symbol symbolTable =
  case typeSize symbolTable symbol of
    Just s -> s
    Nothing -> error "empty stack size???"

-- the symbol table for a given procedure contains everything needed to
-- generate the variable initialisation code for the procedure
generateVariableInitializeCodeForProc :: SymbolTable -> [Oz.ProgramLine]
generateVariableInitializeCodeForProc symbolTable =
  concat $
    map
      (generateVariableInitializeCode symbolTable)
      (localVars symbolTable)

generateVariableInitializeCode :: SymbolTable -> LocalSymbol -> [Oz.ProgramLine]
generateVariableInitializeCode symbolTable lVarSym@(LocalSymbol (NamedSymbol varIdent varType) _ stackSlotNo) =
  case varType of
    AliasT aliasName passMode ->
      -- Alias types are of variable size, so they will need a variable number
      -- of stack slot initialisations
      let typeStackSize = case typeSize symbolTable lVarSym of
            Just s -> s
            Nothing -> error $ "Variable of type " ++ T.unpack aliasName ++ " has empty stack size???"
        in map Oz.InstructionLine $
            concat $
              [ [Oz.InstrIntConst (Oz.Register 0) (Oz.IntegerConst 0)],
                -- simple approach - just make one stack store/write instruction
                -- for every slot occupied by the type
                [Oz.InstrStore (Oz.StackSlot (stackSlotNo + i)) (Oz.Register 0) | i <- [0..(typeStackSize-1)]]
              ]
    BuiltinT builtinT passMode ->
      let
        initialVal = case passMode of
          Roo.PassByVal -> case builtinT of
            Roo.TBool -> 0
            Roo.TInt -> 0
          Roo.PassByRef -> error "can't have passbyref for local var??"
      in
        map Oz.InstructionLine
          [ Oz.InstrIntConst (Oz.Register 0) (Oz.IntegerConst initialVal),
            Oz.InstrStore (Oz.StackSlot stackSlotNo) (Oz.Register 0)
          ]
    _ -> error ""

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
    otherExpr -> return $ map Oz.InstructionLine (printNotYetImplemented $ "Write for expression type " ++ show otherExpr)
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
    otherExpr -> return $ map Oz.InstructionLine (printNotYetImplemented $ "WriteLn for expression type " ++ show otherExpr)
generateStmtCode _ stmt = do return $ map Oz.InstructionLine (printNotYetImplemented $ "Statement type " ++ show stmt)

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
  return ([Oz.InstrIntConst reg $ Oz.boolConst val], reg)
generateExpEvalCode (Roo.ConstInt _ val) symTable = do
  reg <- getNextRegister
  return ([Oz.InstrIntConst reg $ Oz.IntegerConst val], reg)
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
  naiveWriteBoolConst val ++ naiveWriteNewlineConst

naiveWriteLnIntegerConst :: Integer -> [Oz.Instruction]
naiveWriteLnIntegerConst val =
  naiveWriteIntegerConst val ++ naiveWriteNewlineConst

naiveWriteLnStringConst :: String -> [Oz.Instruction]
naiveWriteLnStringConst val =
  naiveWriteStringConst val ++ naiveWriteNewlineConst

writeValFromRegister :: Oz.Register -> OzIOT -> [Oz.Instruction]
writeValFromRegister reg typ =
  [ Oz.InstrMove (Oz.Register 0) reg,
    case typ of
      OzIOBool -> Oz.InstrCallBuiltin Oz.BuiltinPrintBool
      OzIOInt -> Oz.InstrCallBuiltin Oz.BuiltinPrintInt
      OzIOStr -> Oz.InstrCallBuiltin Oz.BuiltinPrintString
  ]

writeLnValFromRegister :: Oz.Register -> OzIOT -> [Oz.Instruction]
writeLnValFromRegister reg typ =
  writeValFromRegister reg typ ++ naiveWriteNewlineConst

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
