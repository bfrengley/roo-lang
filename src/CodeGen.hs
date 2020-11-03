{-# LANGUAGE FlexibleContexts #-}

module CodeGen where

import qualified AST as Roo
import Analysis
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Writer.Strict (mapWriterT, runWriterT)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import qualified OzAST as Oz
import Semantics
import SymbolTable
import Text.Parsec (SourcePos)
import Text.Parsec.Pos (sourceColumn, sourceLine)
import Util (liftMaybe, liftToMaybeT, mapBoth, whenJust, (=%=))

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
        [] -> Right $ Oz.Program $ runtimeBoilerplate ++ optimiseStores (code finalState)
        _ -> Left errors

optimiseStores :: [Oz.ProgramLine] -> [Oz.ProgramLine]
optimiseStores [] = []
optimiseStores
  ( load@(Oz.InstructionLine (Oz.InstrLoadAddress r1 slot))
      : ( store@(Oz.InstructionLine (Oz.InstrStoreIndirect r2 source))
            : rest
          )
    )
    | r1 == r2 = Oz.InstructionLine (Oz.InstrStore slot source) : optimiseStores rest
    | otherwise = load : store : optimiseStores rest
optimiseStores (line : rest) = line : optimiseStores rest

generateCode :: Roo.Program -> OzState ()
generateCode prog@(Roo.Program _ _ procs) =
  let buildTables =
        ( do
            globalTable <- buildGlobalSymbolTable prog
            mapM (buildLocalSymbolTable globalTable) procs
        )
      -- converts the state within s1 from () to the context-appropriate type (OzStateVal)
      transformStateType s1 = return $ evalState s1 ()
   in do
        tables <- mapWriterT transformStateType buildTables
        zipWithM_ generateProcedureCode tables procs
        writeOutOfBoundsHandler

generateProcedureCode :: SymbolTable -> Roo.Procedure -> OzState ()
generateProcedureCode
  table
  ( Roo.Procedure
      (Roo.ProcHead _ procId args)
      (Roo.ProcBody _ statements)
    ) =
    let -- if it's Nothing there's an error somewhere else
        stackSize = fromMaybe 0 $ calculateStackSize table
        -- try compile each statement and then reset the register between them, since we don't do
        -- any cross-statement optimisation that would require us to preserve register state
        compileProcBody = mapM_ (runMaybeT . compileStmt table >=> const resetRegister) statements
        saveArgsToStack = forM_ [0 .. length args - 1] saveArgumentToStack
     in do
          writeLabel $ procLabel procId
          writeInstr $ Oz.InstrPushStackFrame $ Oz.Framesize stackSize

          when (stackSize > 0) $ do
            saveArgsToStack
            initLocalVars table

          compileProcBody

          writeInstr $ Oz.InstrPopStackFrame (Oz.Framesize stackSize)
          writeInstr Oz.InstrReturn

calculateStackSize :: SymbolTable -> Maybe Int
calculateStackSize table@(SymbolTable _ _ vars) = do
  paramSizes <- mapM (typeSize table) $ orderedSymbols vars
  return $ sum paramSizes

saveArgumentToStack :: Int -> OzState ()
saveArgumentToStack offset =
  -- since parameters can only be of size 1 (only alias types in value mode are size > 1, and
  -- they're not legal as parameters), the stack slot and the register number are the same
  writeInstr $ Oz.InstrStore (Oz.StackSlot offset) $ Oz.Register offset

-- the symbol table for a given procedure contains everything needed to
-- generate the variable initialisation code for the procedure
initLocalVars :: SymbolTable -> OzState ()
initLocalVars symbolTable = do
  -- all default values are 0, so we can just use a single source register
  writeInstr $ Oz.InstrIntConst reservedRegister $ Oz.IntegerConst 0
  mapM_ (runMaybeT . initLocalVar symbolTable) (localVars symbolTable)

initLocalVar :: SymbolTable -> LocalSymbol -> MaybeOzState ()
initLocalVar table lVarSym@(LocalSymbol (NamedSymbol _ _) _ slot) = do
  typeStackSize <- liftMaybe $ typeSize table lVarSym
  writeInstrs
    [ Oz.InstrStore (Oz.StackSlot (slot + i)) reservedRegister
      | i <- [0 .. typeStackSize - 1]
    ]

outOfBoundsHandlerLabel :: Oz.Label
outOfBoundsHandlerLabel = Oz.Label "_err_index_out_of_bounds"

writeOutOfBoundsHandler :: OzState ()
writeOutOfBoundsHandler = do
  writeLabel outOfBoundsHandlerLabel
  writeInstrs
    [ Oz.InstrCallBuiltin Oz.BuiltinPrintString,
      -- we have no way to control the exit code so just exit
      Oz.InstrHalt
    ]

compileStmt :: SymbolTable -> Roo.Stmt -> MaybeOzState ()
compileStmt table (Roo.SAtom pos stmt) = compileAtomicStmt table pos stmt
compileStmt table (Roo.SComp pos stmt) = compileCompStmt table pos stmt

compileCompStmt :: SymbolTable -> SourcePos -> Roo.CompositeStmt -> MaybeOzState ()
compileCompStmt symbolTable sp (Roo.IfBlock testExpr trueStmts falseStmts) =
  let ifIdent = "if_ln" ++ show (sourceLine sp) ++ "_col" ++ show (sourceColumn sp)
      elseLabel = Oz.Label $ ifIdent ++ "_elsebranch"
      endLabel = Oz.Label $ ifIdent ++ "_end"
      evalExpr = compileExpr symbolTable testExpr
      exprT = getExprType symbolTable testExpr
      exprPos = operatorPos testExpr
      hasElse = not $ null falseStmts
   in do
        -- force execution to continue with a dummy register even if compiling the expression fails
        -- we'll rely on an error having been written to catch it later
        writeLabel $ Oz.Label ifIdent -- this isn't necessary (it's not used)
        unless (exprT =%= boolT) $ addError $ InvalidConditionType exprPos exprT boolT
        resultRegister <- lift $ forceCompile reservedRegister evalExpr
        writeInstr $ Oz.InstrBranchOnFalse resultRegister $ if hasElse then elseLabel else endLabel
        mapM_ (compileStmt symbolTable) trueStmts
        -- only generate an else label + code if we actually have an else body
        when hasElse $ do
          writeInstr (Oz.InstrBranchUnconditional endLabel)
          writeLabel elseLabel
          mapM_ (compileStmt symbolTable) falseStmts
        writeLabel endLabel
compileCompStmt symbolTable sp (Roo.WhileBlock testExpr stmts) =
  let whileIdent = "while_ln" ++ show (sourceLine sp) ++ "col" ++ show (sourceColumn sp)
      testLabel = Oz.Label $ whileIdent ++ "_test"
      startLabel = Oz.Label $ whileIdent ++ "_start"
      evalExpr = compileExpr symbolTable testExpr
      exprT = getExprType symbolTable testExpr
      exprPos = operatorPos testExpr
   in do
        -- we compile while loops as if they're do-while loops (with the test after the body), but
        -- with an immediate branch to the test
        -- this lets us avoid an unconditional branch every loop
        writeInstr $ Oz.InstrBranchUnconditional testLabel
        writeLabel startLabel
        mapM_ (compileStmt symbolTable) stmts
        writeLabel testLabel
        unless (exprT =%= boolT) $ addError $ InvalidConditionType exprPos exprT boolT
        resultRegister <- evalExpr
        writeInstr $ Oz.InstrBranchOnTrue resultRegister startLabel

compileAtomicStmt :: SymbolTable -> SourcePos -> Roo.AtomicStmt -> MaybeOzState ()
compileAtomicStmt symbolTable _ (Roo.Write expr) =
  compileWrite symbolTable expr
compileAtomicStmt symbolTable _ (Roo.WriteLn expr) = do
  compileWrite symbolTable expr
  lift $ writeStringConst "\\n"
compileAtomicStmt symbolTable pos (Roo.Read lValue) =
  let lValType = getLvalType symbolTable lValue
   in do
        readBuiltin <- case lValType of
          (BuiltinT Roo.TBool _) -> return Oz.BuiltinReadBool
          (BuiltinT Roo.TInt _) -> return Oz.BuiltinReadInt
          t -> addError (InvalidReadType pos t [boolT, intT]) >> failCompile
        writeInstr $ Oz.InstrCallBuiltin readBuiltin
        dest <- compileLvalLoad Roo.PassByRef symbolTable lValue
        writeInstr $ Oz.InstrStoreIndirect dest reservedRegister
compileAtomicStmt table pos (Roo.Assign lval@(Roo.LValue _ varId _ _) expr) =
  let exprT = getExprType table expr
      lvalT = getLvalType table lval
      -- this fromJust should be safe because of laziness
      -- if the var doesn't exist its type is UnknownT and the unless condition in the first error
      -- will be true because UnknownT is equal to any type, so it won't be evaluated
      -- the when will never be reached when the var doesn't exist because `isAliasT UnknownT`
      -- is false
      varDecl = getIdent $ fromJust $ lookupVar table $ getName varId
      assignError = InvalidAssign pos varDecl lvalT exprT
   in do
        unless (exprT =%= lvalT) $ addError assignError
        if isAliasT lvalT
          then do
            when (exprT /= UnknownT && lvalT /= exprT) $ addError assignError
            case expr of
              Roo.LVal _ sourceLval -> compileAliasTypeAssign table lval sourceLval
              -- invalid, but compile it anyway to check the lvalue/expression semantics
              _ -> compileExprAssign table lval expr >> failCompile
          else compileExprAssign table lval expr
compileAtomicStmt table _ (Roo.Call procId paramExprs) =
  let (Roo.Ident callsite _) = procId
   in case lookupProcedure table (getName procId) of
        Nothing -> addError (UnknownProcedure procId) >> failCompile
        Just (ProcSymbol declId paramSymbols) -> do
          lift $ prepareArgs table callsite declId 0 paramSymbols paramExprs
          writeInstr $ Oz.InstrCall (procLabel procId)

prepareArgs ::
  SymbolTable ->
  -- The callsite (the source position at which the procedure call appears), for error messages
  SourcePos ->
  -- The Ident from the procedure declaration, for error messages
  Roo.Ident ->
  -- The cumulative count of handled args, for argument count mismatch errors
  Int ->
  -- The list of parameter symbols
  [NamedSymbol] ->
  -- The list of expressions supplied at the callsite
  [Roo.Expr] ->
  -- A list of registers in which each argument is stored
  OzState ()
prepareArgs _ _ _ _ [] [] = return () -- base case for matching arg count
prepareArgs _ callsite declId count [] exprList =
  -- more args than params
  addError (ArgumentCountMismatch callsite declId (count + length exprList) count)
prepareArgs _ callsite declId count paramList [] =
  -- more params than args
  addError (ArgumentCountMismatch callsite declId count (count + length paramList))
prepareArgs table callsite declId count (param : paramList) (expr : exprList) =
  let paramT = symbolType param
      exprT = getExprType table expr
      typeErr = ArgumentTypeMismatch (Roo.exprStart expr) (getIdent param) exprT paramT
   in do
        unless (paramT =%= exprT) $ addError typeErr
        reg <- runMaybeT $ case getPassMode param of
          Roo.PassByRef -> case expr of
            -- only lvalue expressions can ever be in reference mode, since other expressions don't
            -- have an address
            Roo.LVal _ lval -> compileLvalLoad Roo.PassByRef table lval
            _ -> addError typeErr >> failCompile
          Roo.PassByVal -> compileExpr table expr
        whenJust reg $ writeInstr . Oz.InstrMove (Oz.Register count)
        prepareArgs table callsite declId (count + 1) paramList exprList

getPassMode :: NamedSymbol -> Roo.ProcParamPassMode
getPassMode (NamedSymbol _ symT) = case symT of
  AliasT _ mode -> mode
  BuiltinT _ mode -> mode
  -- don't print an error here - we should have caught it when building the symbol table
  -- just pretend like we have a real mode, I don't think it matters (for now)
  _ -> Roo.PassByVal

compileExprAssign :: SymbolTable -> Roo.LValue -> Roo.Expr -> MaybeOzState ()
compileExprAssign table lval expr = do
  -- compile both sides without bailing on failure (yet) to record errors
  let maybeDest = runMaybeT $ compileLvalLoad Roo.PassByRef table lval
  let maybeSource = runMaybeT $ compileExpr table expr
  source <- liftToMaybeT maybeSource
  dest <- liftToMaybeT maybeDest
  writeInstr $ Oz.InstrStoreIndirect dest source

compileAliasTypeAssign :: SymbolTable -> Roo.LValue -> Roo.LValue -> MaybeOzState ()
compileAliasTypeAssign table destLval sourceLval =
  let -- extract the underlying alias type which the lvalue is a reference to so we can get its size
      extractBaseAliasType lval = case getLvalType table lval of
        t@(AliasT _ _) -> Just $ setMode Roo.PassByVal t
        _ -> Nothing
      sourceSize = extractBaseAliasType sourceLval >>= typeSize table
      destSize = extractBaseAliasType destLval >>= typeSize table
   in do
        let maybeDest = runMaybeT $ compileLvalLoad Roo.PassByRef table destLval
        let maybeSource = runMaybeT $ compileLvalLoad Roo.PassByRef table sourceLval
        dest <- liftToMaybeT maybeDest
        source <- liftToMaybeT maybeSource
        -- remap the types to the underlying types
        sourceSize' <- liftMaybe sourceSize
        destSize' <- liftMaybe destSize
        if sourceSize' /= destSize'
          then failCompile
          else do
            incrReg <- getNextRegister
            copyReg <- getNextRegister
            -- explicitly write the first load and the write the rest via a loop
            -- this lets us avoid two extra sub_offsets
            writeInstrs
              [ Oz.InstrIntConst incrReg $ Oz.IntegerConst 1,
                Oz.InstrLoadIndirect copyReg source,
                Oz.InstrStoreIndirect dest copyReg
              ]
            mapM_
              ( \_ ->
                  writeInstrs
                    [ Oz.InstrSubOffset source source incrReg,
                      Oz.InstrSubOffset dest dest incrReg,
                      Oz.InstrLoadIndirect copyReg source,
                      Oz.InstrStoreIndirect dest copyReg
                    ]
              )
              [1 .. sourceSize' - 1]

compileWrite :: SymbolTable -> Roo.Expr -> MaybeOzState ()
compileWrite _ (Roo.ConstBool _ bool) = lift $ writeBoolConst bool
compileWrite _ (Roo.ConstInt _ int) = lift $ writeIntegerConst int
compileWrite _ (Roo.ConstStr _ str) = lift $ writeStringConst str
compileWrite table expr =
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
compileExpr table expr = let expr' = propagateConsts expr in compileExpr' table expr'

compileExpr' :: SymbolTable -> Roo.Expr -> MaybeOzState Oz.Register
compileExpr' _ (Roo.ConstBool _ val) = do
  reg <- getNextRegister
  writeInstr $ Oz.InstrIntConst reg (Oz.boolConst val)
  return reg
compileExpr' _ (Roo.ConstInt _ val) = do
  reg <- getNextRegister
  writeInstr $ Oz.InstrIntConst reg (Oz.IntegerConst val)
  return reg
compileExpr' _ (Roo.ConstStr _ val) = do
  reg <- getNextRegister
  writeInstr $ Oz.InstrStringConst reg (Oz.StringConst val)
  return reg
compileExpr' table (Roo.UnOpExpr _ op expr) = do
  lift $ expectUnOpType op (operatorPos expr) $ getExprType table expr

  source <- compileExpr' table expr
  dest <- getNextRegister
  writeInstr $ unOpInstruction op dest source
  return dest
compileExpr' table (Roo.BinOpExpr pos op left right) = do
  let leftT = getExprType table left
  lift $ expectBinOpType op (operatorPos left) leftT
  let rightT = getExprType table right
  lift $ expectBinOpType op (operatorPos right) rightT
  unless (leftT =%= rightT) $ addError (BinaryTypeMismatch pos op leftT rightT)

  -- forcibly evaluate both sides of the expression before we possibly fail out
  -- if we just did `leftReg <- compileExpr table left`, it might fail and therefore exit early
  -- without checking the right hand side of the expression, which means we'd only get errors
  -- for the left hand side
  let leftReg = runMaybeT $ compileExpr' table left
  let rightReg = runMaybeT $ compileExpr' table right
  leftReg' <- liftToMaybeT leftReg
  rightReg' <- liftToMaybeT rightReg
  dest <- getNextRegister
  writeInstr $ binOpInstruction op dest leftReg' rightReg'
  return dest
compileExpr' table (Roo.LVal _ lval) = compileLvalLoad Roo.PassByVal table lval

propagateConsts :: Roo.Expr -> Roo.Expr
propagateConsts expr@(Roo.UnOpExpr pos op _) = case unOpReturnType op of
  t
    | t =%= intT -> evalIntExpr pos expr
    | t =%= boolT -> evalBoolExpr pos expr
    | otherwise -> expr
propagateConsts expr@(Roo.BinOpExpr pos op _ _) = case binOpReturnType op of
  t
    | t =%= intT -> evalIntExpr pos expr
    | t =%= boolT -> evalBoolExpr pos expr
    | otherwise -> expr
propagateConsts expr = expr

evalIntExpr :: SourcePos -> Roo.Expr -> Roo.Expr
evalIntExpr pos expr = case evalIntExpr' expr of
  Left e -> e
  Right i -> Roo.ConstInt pos i

evalIntExpr' :: Roo.Expr -> Either Roo.Expr Integer
evalIntExpr' (Roo.ConstInt _ i) = Right i
evalIntExpr' expr@(Roo.UnOpExpr _ Roo.OpNeg expr') =
  mapBoth (const expr) negate $ evalIntExpr' expr'
evalIntExpr' expr@(Roo.BinOpExpr _ op left right) =
  let left' = evalIntExpr' left
      right' = evalIntExpr' right
   in case (left', right') of
        (Right i, Right j) -> case op of
          Roo.OpMul -> Right $ i * j
          Roo.OpMinus -> Right $ i - j
          Roo.OpPlus -> Right $ i + j
          Roo.OpDiv -> if j /= 0 then Right (i `div` j) else Left expr
        _ -> Left expr
evalIntExpr' expr = Left expr

evalBoolExpr :: SourcePos -> Roo.Expr -> Roo.Expr
evalBoolExpr pos expr = case evalBoolExpr' expr of
  Left e -> e
  Right b -> Roo.ConstBool pos b

evalBoolExpr' :: Roo.Expr -> Either Roo.Expr Bool
evalBoolExpr' (Roo.ConstBool _ b) = Right b
evalBoolExpr' expr@(Roo.UnOpExpr _ Roo.OpNot expr') = mapBoth (const expr) not $ evalBoolExpr' expr'
evalBoolExpr' expr@(Roo.BinOpExpr pos op left right) =
  let left' = propagateConsts left
      right' = propagateConsts right
   in -- if/else is ugly in haskell, so I'll just let GHC optimise this one...
      case () of
        () | isBooleanOp op -> case (left', right') of
          (Roo.ConstBool _ b, Roo.ConstBool _ b') -> case op of
            Roo.OpAnd -> Right $ b && b'
            Roo.OpOr -> Right $ b || b'
            _ -> Left expr
          _ -> Left $ Roo.BinOpExpr pos op left' right'
        () | isRelOp op -> case (left', right') of
          (Roo.ConstBool _ b, Roo.ConstBool _ b') -> Right $ evalRelOp op b b'
          (Roo.ConstInt _ i, Roo.ConstInt _ i') -> Right $ evalRelOp op i i'
          _ -> Left $ Roo.BinOpExpr pos op left' right'
        () | otherwise -> Left expr
evalBoolExpr' expr = Left expr

evalRelOp :: (Ord a) => Roo.BinaryOp -> a -> a -> Bool
evalRelOp Roo.OpEq l r = l == r
evalRelOp Roo.OpNeq l r = l /= r
evalRelOp Roo.OpGreater l r = l > r
evalRelOp Roo.OpGreaterEq l r = l >= r
evalRelOp Roo.OpLess l r = l < r
evalRelOp Roo.OpLessEq l r = l <= r

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
        (ident, size, elemT) <-
          ( case baseT of
              Just baseT'@(AliasT name _) -> case lookupType table name of
                Just (ArrayT ident size elemT') -> return (ident, toInteger size, elemT')
                Just (RecordT (Roo.Ident pos' _) _) ->
                  addError (UnexpectedIndex pos varIdent baseT' (Just pos')) >> failCompile
                _ -> failCompile
              Just baseT' -> addError (UnexpectedIndex pos varIdent baseT' Nothing) >> failCompile
              Nothing -> failCompile
            )
        let expr' = propagateConsts expr
        lift $ expectType intT (InvalidIndexType pos) $ getExprType table expr'
        case expr' of
          Roo.ConstInt _ i -> do
            when (i >= size || i < 0) $ addError $ IndexOutOfBounds pos i size varIdent ident
            when (i /= 0) $ do
              offsetReg <- getNextRegister
              writeInstrs
                [ Oz.InstrIntConst offsetReg $ Oz.IntegerConst i,
                  Oz.InstrSubOffset dest dest offsetReg
                ]
            return elemT
          _ -> do
            offsetReg <- compileExpr table expr'
            lift $ writeBoundsCheck pos offsetReg size
            writeInstr $ Oz.InstrSubOffset dest dest offsetReg
            return elemT

writeBoundsCheck :: SourcePos -> Oz.Register -> Integer -> OzState ()
writeBoundsCheck sp idxReg size =
  let okLabel = Oz.Label $ "inbounds_ln" ++ show (sourceLine sp) ++ "_col" ++ show (sourceColumn sp)
   in do
        sizeReg <- getNextRegister
        zeroReg <- getNextRegister
        cmpReg <- getNextRegister
        writeInstrs
          [ Oz.InstrIntConst sizeReg $ Oz.IntegerConst size,
            Oz.InstrIntConst zeroReg $ Oz.IntegerConst 0,
            Oz.InstrCmpGreaterEqualInt zeroReg idxReg zeroReg, -- idx >= 0
            Oz.InstrCmpLessThanInt cmpReg idxReg sizeReg, -- idx < size
            Oz.InstrAnd cmpReg cmpReg zeroReg, -- 0 <= idx < size
            -- if the above is true, it's inbounds
            Oz.InstrBranchOnTrue cmpReg okLabel,
            -- if we're here, it's an out-of-bounds index
            Oz.InstrStringConst reservedRegister $
              Oz.StringConst $
                T.unpack (writePos sp) ++ ": index out-of-bounds\\n",
            Oz.InstrBranchUnconditional outOfBoundsHandlerLabel
          ]
        writeLabel okLabel

compileFieldAccess :: SymbolTable -> Oz.Register -> Roo.Ident -> SymbolType -> Maybe Roo.Ident -> MaybeOzState ()
compileFieldAccess _ _ _ _ Nothing = return ()
compileFieldAccess table dest varIdent baseT (Just fieldIdent@(Roo.Ident pos _)) = case baseT of
  AliasT name _ -> case lookupType table name of
    Just (RecordT typeIdent fields) -> case Map.lookup (getName fieldIdent) fields of
      Just (FieldSymbol _ idx) ->
        when (idx /= 0) $ do
          reg <- getNextRegister
          writeInstrs
            [ Oz.InstrIntConst reg $ Oz.IntegerConst $ toInteger idx,
              Oz.InstrSubOffset dest dest reg
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

writeBoolConst :: Bool -> OzState ()
writeBoolConst val =
  writeInstrs
    [ Oz.InstrIntConst reservedRegister (Oz.boolConst val),
      Oz.InstrCallBuiltin Oz.BuiltinPrintBool
    ]

writeIntegerConst :: Integer -> OzState ()
writeIntegerConst val =
  writeInstrs
    [ Oz.InstrIntConst reservedRegister (Oz.IntegerConst val),
      Oz.InstrCallBuiltin Oz.BuiltinPrintInt
    ]

writeStringConst :: String -> OzState ()
writeStringConst val =
  writeInstrs
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

forceCompile :: Monad m => a -> MaybeT m a -> m a
forceCompile defaultVal action = fromMaybe defaultVal <$> runMaybeT action
