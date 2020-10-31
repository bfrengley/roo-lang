{-# LANGUAGE FlexibleContexts #-}

module CodeGen where

import qualified AST as Roo
import Analysis
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
-- import Data.Map.Ordered

import Control.Monad.Trans.Writer.Strict (mapWriterT, runWriterT)
import Data.Either (lefts, rights)
import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified OzAST as Oz
import Semantics
import SymbolTable
import Util (liftMaybe, (=%=))

data OzIOT = OzIOInt | OzIOBool | OzIOStr

data RooWriteT = RooWrite | RooWriteLn

data OzStateVal = OzStateVal
  { register :: Oz.Register,
    code :: [Oz.ProgramLine]
  }

initialState :: OzStateVal
initialState = OzStateVal {register = initialRegister, code = []}

-- track the lowest free register
type OzState = SemanticState OzStateVal

type MaybeOzState = MaybeT OzState

runtimeBoilerplate :: [Oz.ProgramLine]
runtimeBoilerplate =
  map
    Oz.InstructionLine
    [ Oz.InstrCall (Oz.Label "main"),
      Oz.InstrHalt
    ]

compileRooProgram :: Roo.Program -> Either [SemanticError] Oz.Program
compileRooProgram program =
  let ((_, errors), finalState) = runState (runWriterT $ generateCode program) initialState
   in case errors of
        [] -> Right $ Oz.Program $ runtimeBoilerplate ++ code finalState
        _ -> Left errors

generateCode :: Roo.Program -> OzState ()
generateCode prog@(Roo.Program _ _ procs) =
  let tables =
        ( do
            globalTable <- buildGlobalSymbolTable prog
            mapM (buildLocalSymbolTable globalTable) procs
        )
   in do
        tables' <- mapWriterT transformStateType tables
        zipWithM_ generateProcedureCode tables' procs

transformStateType :: State () a -> State OzStateVal a
transformStateType s1 = return $ evalState s1 ()

generateProcedureCode :: SymbolTable -> Roo.Procedure -> OzState ()
generateProcedureCode symbolTable procedure@(Roo.Procedure (Roo.ProcHead _ (Roo.Ident _ procName) args) (Roo.ProcBody varDecls statements)) = do
  writeLabel $ Oz.Label procName
  writeInstrs stackSetup
  instrs
  writeInstrs $
    concat
      [ stackCleanup,
        [Oz.InstrReturn]
      ]
  where
    stackSize = case calculateStackSize symbolTable of
      Just size -> size
      Nothing -> error $ "Stack size calculation for procedure " ++ procName ++ " failed"
    instrs = (mapM (generateStmtCode symbolTable) statements)
    stackSetup =
      if stackSize > 0
        then
          let saveArgs = zipWith (\i arg -> generateArgumentStackSaveInstr (Oz.Register i) 0) [0 ..] args
              initVars = generateLocalVariableInitializeCodeForProc symbolTable
           in concat
                [ [Oz.InstrPushStackFrame $ Oz.Framesize stackSize],
                  saveArgs,
                  initVars
                ]
        else []
    stackCleanup =
      if stackSize > 0
        then [Oz.InstrPopStackFrame (Oz.Framesize stackSize)]
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
generateLocalVariableInitializeCodeForProc :: SymbolTable -> [Oz.Instruction]
generateLocalVariableInitializeCodeForProc symbolTable =
  concatMap
    (generateLocalVariableInitializeCode symbolTable)
    (localVars symbolTable)

generateLocalVariableInitializeCode :: SymbolTable -> LocalSymbol -> [Oz.Instruction]
generateLocalVariableInitializeCode symbolTable lVarSym@(LocalSymbol (NamedSymbol varIdent varType) _ stackSlotNo) =
  case varType of
    AliasT aliasName passMode ->
      -- Alias types are of variable size, so they will need a variable number
      -- of stack slot initialisations
      let typeStackSize = case typeSize symbolTable lVarSym of
            Just s -> s
            Nothing -> error $ "Variable of type " ++ T.unpack aliasName ++ " has empty stack size???"
       in concat $
            [ [Oz.InstrIntConst (Oz.Register 0) (Oz.IntegerConst 0)],
              -- simple approach - just make one stack store/write instruction
              -- for every slot occupied by the type
              [Oz.InstrStore (Oz.StackSlot (stackSlotNo + i)) (Oz.Register 0) | i <- [0 .. (typeStackSize -1)]]
            ]
    BuiltinT builtinT passMode ->
      let initialVal = case passMode of
            Roo.PassByVal -> case builtinT of
              Roo.TBool -> 0
              Roo.TInt -> 0
            Roo.PassByRef -> error "can't have passbyref for local var??"
       in [ Oz.InstrIntConst (Oz.Register 0) (Oz.IntegerConst initialVal),
            Oz.InstrStore (Oz.StackSlot stackSlotNo) (Oz.Register 0)
          ]
    _ -> error ""

generateStmtCode :: SymbolTable -> Roo.Stmt -> OzState ()
generateStmtCode symbolTable (Roo.SAtom _ (Roo.Write expr)) =
  generateWriteStmtCode symbolTable expr
generateStmtCode symbolTable (Roo.SAtom _ (Roo.WriteLn expr)) = do
  generateWriteStmtCode symbolTable expr
  writeInstrs
    [ Oz.InstrStringConst reservedRegister $ Oz.StringConst "\n",
      Oz.InstrCallBuiltin Oz.BuiltinPrintString
    ]
generateStmtCode symbolTable (Roo.SAtom _ (Roo.Call (Roo.Ident _ procIdent) paramExprs)) = do
  let (ProcSymbol _ paramSymbols) = case lookupProcedure symbolTable (T.pack procIdent) of
        Just sym -> sym
        Nothing -> error $ "couldn't find proc with ident " ++ procIdent ++ " in symbol table???"
      procArgPassTypes =
        map
          ( \(NamedSymbol paramIdent paramType) -> case paramType of
              (AliasT _ passMode) -> passMode
              (BuiltinT _ passMode) -> passMode
              _ -> error $ "type " ++ show paramType ++ " can't be used as argument for param " ++ show paramIdent
          )
          paramSymbols
  paramSources <-
    mapM
      ( \(expr, passType) -> case passType of
          -- For PassByRef params the LValue is needed
          Roo.PassByRef -> case expr of
            Roo.LVal _ lValue -> do
              return $ Right lValue
            _ -> error $ "invalid expr " ++ show expr ++ " for PassByRef param"
          -- For PassByVal params, the expression must be evaluated
          Roo.PassByVal -> do
            exprEvalCode <- generateExpEvalCode expr symbolTable
            return $ Left exprEvalCode
      )
      (zip paramExprs procArgPassTypes)
  let paramExprCalculationInstrs = map fst (lefts paramSources)
      argPrepStsmts =
        map
          ( \(paramSource, procParamIndex) ->
              let destRegister = (Oz.Register procParamIndex)
               in case paramSource of
                    Left (_, reg) -> Oz.InstrMove destRegister reg
                    Right _ -> Oz.InstrLoadAddress destRegister (Oz.StackSlot 0) -- XXX: todo! lValue stack slot lookup...
          )
          (zip paramSources [0 ..])
      stmts = (concat paramExprCalculationInstrs) ++ argPrepStsmts ++ [Oz.InstrCall (Oz.Label procIdent)]
  writeInstrs stmts
generateStmtCode _ stmt = writeInstrs (printNotYetImplemented $ "Statement type " ++ show stmt)

generateWriteStmtCode :: SymbolTable -> Roo.Expr -> OzState ()
generateWriteStmtCode _ (Roo.ConstBool _ bool) = writeInstrs $ naiveWriteBoolConst bool
generateWriteStmtCode _ (Roo.ConstInt _ int) = writeInstrs $ naiveWriteIntegerConst int
generateWriteStmtCode _ (Roo.ConstStr _ str) = writeInstrs $ naiveWriteStringConst str
generateWriteStmtCode table expr = do
  v <- runMaybeT $ generateExprEvalWriteCode table expr
  return $ fromMaybe () v

generateExprEvalWriteCode :: SymbolTable -> Roo.Expr -> MaybeOzState ()
generateExprEvalWriteCode symbolTable expr = do
  exprReg <- compileExpr symbolTable expr
  let writeExprOutputInstrs = writeValFromRegister exprReg (ioTypeFromExpr expr symbolTable)
  writeInstrs writeExprOutputInstrs

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
generateExpEvalCode :: Roo.Expr -> SymbolTable -> OzState ([Oz.Instruction], Oz.Register)
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

compileExpr :: SymbolTable -> Roo.Expr -> MaybeOzState Oz.Register
compileExpr _ (Roo.ConstBool _ val) = do
  reg <- getNextRegister
  writeInstr $ Oz.InstrIntConst reg (Oz.boolConst val)
  return reg
compileExpr _ (Roo.ConstInt _ val) = do
  reg <- getNextRegister
  writeInstr $ Oz.InstrIntConst reg (Oz.IntegerConst val)
  return reg
compileExpr _ (Roo.ConstStr _ val) = do
  reg <- getNextRegister
  writeInstr $ Oz.InstrStringConst reg (Oz.StringConst val)
  return reg
compileExpr table (Roo.UnOpExpr _ op expr) = do
  lift $ expectUnOpType op (getPos expr) $ getExprType table expr

  source <- compileExpr table expr
  dest <- getNextRegister
  writeInstr $ unOpInstruction op dest source
  return dest
compileExpr table (Roo.BinOpExpr pos op left right) = do
  let leftT = getExprType table left
  lift $ expectBinOpType op (getPos left) leftT
  let rightT = getExprType table right
  lift $ expectBinOpType op (getPos right) rightT
  unless (leftT =%= rightT) $ addError (BinaryTypeMismatch pos op leftT rightT)

  let leftReg = runMaybeT $ compileExpr table left
  let rightReg = runMaybeT $ compileExpr table right
  leftReg' <- lift leftReg >>= liftMaybe
  rightReg' <- lift rightReg >>= liftMaybe
  dest <- getNextRegister
  writeInstr $ binOpInstruction op dest leftReg' rightReg'
  return dest
compileExpr table (Roo.LVal _ lval) = compileLvalLoad Roo.PassByVal table lval

compileLvalLoad :: Roo.ProcParamPassMode -> SymbolTable -> Roo.LValue -> MaybeOzState Oz.Register
-- load the value of a plain variable
compileLvalLoad Roo.PassByVal table (Roo.LValue _ ident Nothing Nothing) = do
  var <- checkedLookupVar table ident
  case symbolType var of
    -- an alias type can never be loaded in value mode
    AliasT _ _ -> addError (AliasLoadInValueMode ident) >> liftMaybe Nothing
    BuiltinT _ mode -> do
      dest <- getNextRegister
      let sourceSlot = ozStackSlot var
      case mode of
        -- the stack slot contains a value, so load it directly
        Roo.PassByVal -> writeInstr $ Oz.InstrLoad dest sourceSlot
        Roo.PassByRef -> do
          addrReg <- getNextRegister
          writeInstrs
            [ -- load the address the value is stored in
              Oz.InstrLoad addrReg sourceSlot,
              -- indirectly load the value at that address
              Oz.InstrLoadIndirect dest addrReg
            ]
      return dest
    _ -> liftMaybe Nothing
compileLvalLoad Roo.PassByRef table (Roo.LValue _ ident Nothing Nothing) = do
  var <- checkedLookupVar table ident
  mode <- liftMaybe $ case symbolType var of
    AliasT _ mode -> Just mode
    BuiltinT _ mode -> Just mode
    _ -> Nothing
  dest <- getNextRegister
  let sourceSlot = ozStackSlot var
  case mode of
    -- the stack slot contains a value, so load the address of the stack slot
    Roo.PassByVal -> writeInstr $ Oz.InstrLoadAddress dest sourceSlot
    -- the stack slot contains an address, so load that directly
    Roo.PassByRef -> writeInstr $ Oz.InstrLoad dest sourceSlot
  return dest
compileLvalLoad mode table (Roo.LValue pos ident index field) = do
  baseAddrReg <- compileLvalLoad Roo.PassByRef table (Roo.LValue pos ident Nothing Nothing)

  return $ Oz.Register 1

compileIndexExpr :: SymbolTable -> SymbolType -> Oz.Register -> Roo.Expr -> MaybeOzState Oz.Register
compileIndexExpr table baseT dest expr = do
  lift $ expectType intT (InvalidIndexType (Roo.exprPos expr)) $ getExprType table expr
  let elemT =
        ( case baseT of
            AliasT name _ -> case lookupType table name of
              Just (ArrayT _ _ elemT') -> elemT'
              _ -> UnknownT
            _ -> UnknownT
        )
  offsetReg <- compileExpr table expr
  -- is sub correct?
  writeInstr $ Oz.InstrSubOffset dest dest offsetReg
  return dest

checkedLookupVar :: SymbolTable -> Roo.Ident -> MaybeOzState LocalSymbol
checkedLookupVar table ident = case lookupVar table (getName ident) of
  Just v -> return v
  Nothing -> addError (UnknownVar ident) >> liftMaybe Nothing

ozStackSlot :: LocalSymbol -> Oz.StackSlot
ozStackSlot (LocalSymbol _ _ slot) = Oz.StackSlot slot

writeInstr :: MonadState OzStateVal m => Oz.Instruction -> m ()
writeInstr instr = writeInstrs [instr]

writeInstrs :: MonadState OzStateVal m => [Oz.Instruction] -> m ()
writeInstrs instrs = modify (\st -> st {code = code st ++ map Oz.InstructionLine instrs})

writeLabel :: MonadState OzStateVal m => Oz.Label -> m ()
writeLabel label = modify (\st -> st {code = code st ++ [Oz.LabelLine label]})

printNotYetImplemented :: String -> [Oz.Instruction]
printNotYetImplemented stmntDescription = naiveWriteStringConst (show (stmntDescription ++ " not yet implemented\n"))

naiveWriteBoolConst :: Bool -> [Oz.Instruction]
naiveWriteBoolConst val =
  [ Oz.InstrIntConst reservedRegister (Oz.boolConst val),
    Oz.InstrCallBuiltin Oz.BuiltinPrintBool
  ]

naiveWriteIntegerConst :: Integer -> [Oz.Instruction]
naiveWriteIntegerConst val =
  [ Oz.InstrIntConst reservedRegister (Oz.IntegerConst val),
    Oz.InstrCallBuiltin Oz.BuiltinPrintInt
  ]

naiveWriteStringConst :: String -> [Oz.Instruction]
naiveWriteStringConst val =
  [ Oz.InstrStringConst reservedRegister (Oz.StringConst val),
    Oz.InstrCallBuiltin Oz.BuiltinPrintString
  ]

writeValFromRegister :: Oz.Register -> OzIOT -> [Oz.Instruction]
writeValFromRegister reg typ =
  [ Oz.InstrMove reservedRegister reg,
    case typ of
      OzIOBool -> Oz.InstrCallBuiltin Oz.BuiltinPrintBool
      OzIOInt -> Oz.InstrCallBuiltin Oz.BuiltinPrintInt
      OzIOStr -> Oz.InstrCallBuiltin Oz.BuiltinPrintString
  ]

(|+|) :: Oz.Register -> Int -> Oz.Register
(Oz.Register r) |+| n = Oz.Register $ r + n

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

getNextRegister :: MonadState OzStateVal m => m Oz.Register
getNextRegister = do
  reg <- gets register
  modify (\st -> st {register = reg |+| 1})
  return reg

-- | 'reservedRegister' is the register reserved by Oz builtin functions (such as for writing and
-- reading). For simplicity, we don't use it except for those purposes (and in procedure calls for
-- argument passing).
reservedRegister :: Oz.Register
reservedRegister = Oz.Register 0

-- | 'initialRegister' is the first register we use for general purposes, which follows the reserved
-- register.
initialRegister :: Oz.Register
initialRegister = reservedRegister |+| 1
