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
import qualified Data.Map.Ordered as OMap
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified OzAST as Oz
import Semantics
import SymbolTable
import Text.Parsec.Pos (sourceLine, sourceColumn)
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
    [ Oz.InstrCall (Oz.Label "proc_main"),
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
generateProcedureCode symbolTable (Roo.Procedure (Roo.ProcHead _ procId@(Roo.Ident _ procName) args) (Roo.ProcBody _ statements)) = do
  writeLabel $ procLabel procId
  writeInstrs stackSetup
  compileProcBody
  when (stackSize > 0) $ writeInstrs stackCleanup
  writeInstr Oz.InstrReturn
  where
    stackSize = case calculateStackSize symbolTable of
      Just size -> size
      Nothing -> error $ "Stack size calculation for procedure " ++ procName ++ " failed"
    -- can you safely reset registers between statements? I think the answer is yes?
    compileProcBody = mapM_ (runMaybeT . generateStmtCode symbolTable >=> const resetRegister) statements
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
    stackCleanup = [Oz.InstrPopStackFrame (Oz.Framesize stackSize)]

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

generateStmtCode :: SymbolTable -> Roo.Stmt -> MaybeOzState ()
generateStmtCode symbolTable (Roo.SAtom _ (Roo.Write expr)) =
  generateWriteStmtCode symbolTable expr
generateStmtCode symbolTable (Roo.SAtom _ (Roo.WriteLn expr)) = do
  generateWriteStmtCode symbolTable expr
  writeInstrs
    [ Oz.InstrStringConst reservedRegister $ Oz.StringConst "\\n",
      Oz.InstrCallBuiltin Oz.BuiltinPrintString
    ]
generateStmtCode table (Roo.SAtom _ (Roo.Call procId paramExprs)) =
  -- let (ProcSymbol _ paramSymbols) = case lookupProcedure symbolTable (T.pack procIdent) of
  --       Just sym -> sym
  --       Nothing -> error $ "couldn't find proc with ident " ++ procIdent ++ " in symbol table???"
  --     procArgPassTypes =
  --       map
  --         ( \(NamedSymbol paramIdent paramType) -> case paramType of
  --             (AliasT _ passMode) -> passMode
  --             (BuiltinT _ passMode) -> passMode
  --             _ -> error $ "type " ++ show paramType ++ " can't be used as argument for param " ++ show paramIdent
  --         )
  --         paramSymbols
  -- paramSources <-
  --   mapM
  --     ( \(expr, passType) -> case passType of
  --         -- For PassByRef params the LValue is needed
  --         Roo.PassByRef -> case expr of
  --           Roo.LVal _ lValue -> do
  --             return $ Right lValue
  --           _ -> error $ "invalid expr " ++ show expr ++ " for PassByRef param"
  --         -- For PassByVal params, the expression must be evaluated
  --         Roo.PassByVal -> do
  --           exprEvalCode <- generateExpEvalCode expr symbolTable
  --           return $ Left exprEvalCode
  --     )
  --     (zip paramExprs procArgPassTypes)
  -- let paramExprCalculationInstrs = map fst (lefts paramSources)
  --     argPrepStsmts =
  --       map
  --         ( \(paramSource, procParamIndex) ->
  --             let destRegister = (Oz.Register procParamIndex)
  --              in case paramSource of
  --                   Left (_, reg) -> Oz.InstrMove destRegister reg
  --                   Right _ -> Oz.InstrLoadAddress destRegister (Oz.StackSlot 0) -- XXX: todo! lValue stack slot lookup...
  --         )
  --         (zip paramSources [0 ..])
  --     stmts = (concat paramExprCalculationInstrs) ++ argPrepStsmts ++ [Oz.InstrCall (Oz.Label procIdent)]
  -- writeInstrs stmts
  case lookupProcedure table (getName procId) of
    Nothing -> addError (UnknownProcedure procId) >> failCompile
    Just (ProcSymbol _ paramSymbols) -> do
      when (length paramExprs /= length paramSymbols) $ addError ArgumentCountMismatch -- TODO
      registers <-
        zipWithM
          ( \param expr -> case getPassMode param of
              Roo.PassByRef -> case expr of
                -- only lvalue expressions can ever be in reference mode, since other expressions don't
                -- have an address
                Roo.LVal _ lval -> do
                  -- TODO type check
                  compileLvalLoad Roo.PassByRef table lval
                -- can't failCompile here, needs lifted out or we exit early
                _ -> addError ArgumentTypeMismatch >> failCompile
              Roo.PassByVal -> do
                unless (symbolType param =%= getExprType table expr) $ addError ArgumentTypeMismatch
                compileExpr table expr
          )
          paramSymbols
          paramExprs
      zipWithM_ (\dest source -> writeInstr $ Oz.InstrMove (Oz.Register dest) source) [0 ..] registers
      writeInstr $ Oz.InstrCall (procLabel procId)
generateStmtCode symbolTable (Roo.SComp sp (Roo.IfBlock testExpr trueStmts falseStmts)) =
  let ifIdent = "if_ln" ++ show (sourceLine sp) ++ "col" ++ show (sourceColumn sp)
      elseLabel = Oz.Label $ ifIdent ++ "_elsebranch"
      endLabel = Oz.Label $ ifIdent ++ "_end"
      evalExpr = compileExpr symbolTable testExpr
   in do
        -- force execution to continue with a dummy register even if compiling the expression fails
        -- we'll rely on an error having been written to catch it later
        resultRegister <- lift $ forceCompile reservedRegister evalExpr
        writeInstr $ Oz.InstrBranchOnFalse resultRegister elseLabel
        mapM_ (generateStmtCode symbolTable) trueStmts
        writeInstr $ Oz.InstrBranchUnconditional endLabel
        writeLabel elseLabel
        mapM_ (generateStmtCode symbolTable) falseStmts
        writeLabel endLabel
generateStmtCode symbolTable (Roo.SComp sp (Roo.WhileBlock testExpr stmts)) =
  let whileIdent = "while_ln" ++ show (sourceLine sp) ++ "col" ++ show (sourceColumn sp)
      testLabel = Oz.Label $ whileIdent ++ "_test"
      startLabel = Oz.Label $ whileIdent ++ "_start"
      evalExpr = compileExpr symbolTable testExpr
   in do
        -- we compile while loops as if they're do-while loops (with the test after the body), but
        -- with an immediate branch to the test
        -- this lets us avoid an unconditional branch every loop
        writeInstr $ Oz.InstrBranchUnconditional testLabel
        writeLabel startLabel
        mapM_ (generateStmtCode symbolTable) stmts
        writeLabel testLabel
        resultRegister <- evalExpr
        writeInstr $ Oz.InstrBranchOnTrue resultRegister startLabel
generateStmtCode table (Roo.SAtom pos (Roo.Assign lval@(Roo.LValue _ varId _ _) expr)) =
  let exprT = getExprType table expr
      lvalT = getLvalType table lval
      -- this fromJust should be safe because of laziness
      -- if the var doesn't exist its type is UnknownT and the unless condition in the first error
      -- will be true, because UnknownT is equal to any type, so it won't be evaluated
      -- the when condition can never be true when the var doesn't exist because `isAliasT UnknownT`
      -- is false, so it can't be evaluated there if it would fail
      varDecl = getIdent $ fromJust $ lookupVar table $ getName varId
      assignError = InvalidAssign pos varDecl lvalT exprT
   in do
        unless (exprT =%= lvalT) $ addError assignError
        when (isAliasT lvalT && exprT /= UnknownT && lvalT /= exprT) $ addError assignError
        source <- compileExpr table expr
        -- check that reference type lvalues are in reference mode
        -- when (isAliasT lvalT && )
        dest <- compileLvalLoad Roo.PassByRef table lval
        writeInstr $ Oz.InstrStoreIndirect dest source
generateStmtCode _ stmt = writeInstrs (printNotYetImplemented $ "Statement type " ++ show stmt)

getPassMode :: NamedSymbol -> Roo.ProcParamPassMode
getPassMode (NamedSymbol _ symT) = case symT of
  AliasT _ mode -> mode
  BuiltinT _ mode -> mode
  -- don't print an error here - we should have caught it when building the symbol table
  -- just pretend like we have a real mode, I don't think it matters (for now)
  _ -> Roo.PassByVal

forceCompile :: Monad m => a -> MaybeT m a -> m a
forceCompile defaultVal action = fromMaybe defaultVal <$> runMaybeT action

generateWriteStmtCode :: SymbolTable -> Roo.Expr -> MaybeOzState ()
generateWriteStmtCode _ (Roo.ConstBool _ bool) = writeInstrs $ naiveWriteBoolConst bool
generateWriteStmtCode _ (Roo.ConstInt _ int) = writeInstrs $ naiveWriteIntegerConst int
generateWriteStmtCode _ (Roo.ConstStr _ str) = writeInstrs $ naiveWriteStringConst str
generateWriteStmtCode table expr =
  let exprT = getExprType table expr
   in do
        when (isAliasT exprT) $ addError (AliasWrite (Roo.exprStart expr) exprT)
        exprReg <- compileExpr table expr
        writeInstr $ Oz.InstrMove reservedRegister exprReg
        case exprT of
          UnknownT -> failCompile
          StringT -> writeInstr (Oz.InstrCallBuiltin Oz.BuiltinPrintString)
          _ | exprT =%= intT -> writeInstr (Oz.InstrCallBuiltin Oz.BuiltinPrintInt)
          _ | exprT =%= boolT -> writeInstr (Oz.InstrCallBuiltin Oz.BuiltinPrintBool)
          _ -> failCompile

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
  lift $ expectUnOpType op (operatorPos expr) $ getExprType table expr

  source <- compileExpr table expr
  dest <- getNextRegister
  writeInstr $ unOpInstruction op dest source
  return dest
compileExpr table (Roo.BinOpExpr pos op left right) = do
  let leftT = getExprType table left
  lift $ expectBinOpType op (operatorPos left) leftT
  let rightT = getExprType table right
  lift $ expectBinOpType op (operatorPos right) rightT
  unless (leftT =%= rightT) $ addError (BinaryTypeMismatch pos op leftT rightT)

  -- forcibly evaluate both sides of the expression before we possibly fail out
  -- if we just did `leftReg <- compileExpr table left`, it might fail and therefore exit early
  -- without checking the right hand side of the expression, which means we'd only get errors
  -- for the left hand side
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
    t@(AliasT _ _) -> addError (AliasLoadInValueMode ident (getIdent var) t) >> failCompile
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
    _ -> failCompile
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
  -- this is safe because if the variable is unknown we fail the call to `compileLvalLoad`
  let varDeclaration = getIdent $ fromJust $ lookupVar table (getName ident)
  indexT <- compileIndexExpr table baseAddrReg varDeclaration index
  void $ compileFieldAccess table baseAddrReg varDeclaration indexT field
  case mode of
    Roo.PassByRef -> return baseAddrReg
    Roo.PassByVal -> do
      dest <- getNextRegister
      writeInstr $ Oz.InstrLoadIndirect dest baseAddrReg
      return dest

compileIndexExpr :: SymbolTable -> Oz.Register -> Roo.Ident -> Maybe Roo.Expr -> MaybeOzState SymbolType
compileIndexExpr table _ varIdent Nothing =
  liftMaybe $ symbolType <$> lookupVar table (getName varIdent)
compileIndexExpr table dest varIdent (Just expr) =
  let pos = Roo.exprStart expr
      baseT = symbolType <$> lookupVar table (getName varIdent)
   in do
        elemT <-
          ( case baseT of
              Just baseT'@(AliasT name _) -> case lookupType table name of
                Just (ArrayT _ _ elemT') -> return elemT'
                Just (RecordT (Roo.Ident pos' _) _) ->
                  addError (UnexpectedIndex pos varIdent baseT' (Just pos')) >> failCompile
                _ -> failCompile
              Just baseT' -> addError (UnexpectedIndex pos varIdent baseT' Nothing) >> failCompile
              Nothing -> failCompile
            )
        lift $ expectType intT (InvalidIndexType pos) $ getExprType table expr
        offsetReg <- compileExpr table expr
        -- is sub correct?
        writeInstr $ Oz.InstrSubOffset dest dest offsetReg
        return elemT

compileFieldAccess :: SymbolTable -> Oz.Register -> Roo.Ident -> SymbolType -> Maybe Roo.Ident -> MaybeOzState ()
compileFieldAccess _ _ _ _ Nothing = return ()
compileFieldAccess table dest varIdent baseT (Just fieldIdent@(Roo.Ident pos _)) = case baseT of
  AliasT name _ -> case lookupType table name of
    Just (RecordT typeIdent fields) -> case OMap.lookup (getName fieldIdent) fields of
      Just (FieldSymbol _ idx) -> do
        reg <- getNextRegister
        writeInstrs
          [ Oz.InstrIntConst reg $ Oz.IntegerConst $ toInteger idx,
            Oz.InstrAddOffset dest dest reg
          ]
      Nothing -> addError (UnknownField fieldIdent varIdent baseT typeIdent) >> failCompile
    Just (ArrayT (Roo.Ident pos' _) _ _) ->
      addError (UnexpectedField pos varIdent baseT (Just pos')) >> failCompile
    _ -> failCompile
  _ -> addError (UnexpectedField pos varIdent baseT Nothing) >> failCompile

checkedLookupVar :: SymbolTable -> Roo.Ident -> MaybeOzState LocalSymbol
checkedLookupVar table ident = case lookupVar table (getName ident) of
  Just v -> return v
  Nothing -> addError (UnknownVar ident) >> failCompile

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

procLabel :: Roo.Ident -> Oz.Label
procLabel (Roo.Ident _ name) = Oz.Label ("proc_" ++ name)

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

resetRegister :: MonadState OzStateVal m => m ()
resetRegister = modify (\st -> st {register = initialRegister})

failCompile :: Monad m => MaybeT m a
failCompile = liftMaybe Nothing

ozStackSlot :: LocalSymbol -> Oz.StackSlot
ozStackSlot (LocalSymbol _ _ slot) = Oz.StackSlot slot

writeInstr :: MonadState OzStateVal m => Oz.Instruction -> m ()
writeInstr instr = writeInstrs [instr]

writeInstrs :: MonadState OzStateVal m => [Oz.Instruction] -> m ()
writeInstrs instrs = modify (\st -> st {code = code st ++ map Oz.InstructionLine instrs})

writeLabel :: MonadState OzStateVal m => Oz.Label -> m ()
writeLabel label = modify (\st -> st {code = code st ++ [Oz.LabelLine label]})
