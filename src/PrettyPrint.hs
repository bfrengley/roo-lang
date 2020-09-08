module PrettyPrint where

import AST
import Control.Monad.State.Lazy
import Data.Maybe (maybe)

-- prettyPrint :: Program -> String
-- prettyPrint rootNode =
--     case rootNode of
--         Procedure -> "[Procedure]"

-- prettyPrint :: Program -> IO ()
-- prettyPrint (Program records arrs procs) =
--   do
--     map printRecord records

-- printRecord :: Record -> IO ()
-- printRecord record =
--   firstRec:nextRecs
--   where firstRec:nextRecs = records

prettyPrint :: Program -> String
prettyPrint (Program recordDefs arrayDefs mainProc) =
  unlines
    [ unlines recLines,
      unlines arrayLines,
      unlines mainProcLines
    ]
  where
    recLines = ["sample", "    rec", "    lines"]
    arrayLines = ["sample", "    array", "    lines"]
    mainProcLines = ["sample", "    main", "    lines"]

-- case rootNode of
--     Procedure -> "[Procedure]"

--
-- Expression pretty printer
--
-- It has issues with
--

initialPrecedence :: Int
initialPrecedence = 0

binOpPrecedence :: BinaryOp -> Int
binOpPrecedence OpOr = 1
binOpPrecedence OpAnd = 2
binOpPrecedence OpEq = 4
binOpPrecedence OpNeq = 4
binOpPrecedence OpLess = 4
binOpPrecedence OpLessEq = 4
binOpPrecedence OpGreater = 4
binOpPrecedence OpGreaterEq = 4
binOpPrecedence OpPlus = 5
binOpPrecedence OpMinus = 5
binOpPrecedence OpMul = 6
binOpPrecedence OpDiv = 6

-- the operators * and / have the same precedence, but / isn't associative
-- these special sentinel values allow us to detect when a mul/div expression occurs on the RHS of
-- a * or / so that we can keep the parentheses in place
rhsMulPrecedence :: Int
rhsMulPrecedence = 7

rhsDivPrecedence :: Int
rhsDivPrecedence = 8

unOpPrecedence :: UnaryOp -> Int
unOpPrecedence OpNot = 3
-- this is higher than expected to allow us to do special handling of mul/div associativity
unOpPrecedence OpNeg = 9

pPrintLval :: LValue -> String
pPrintLval (LValue ident idx field) =
  let showIndex i = "[" ++ pPrintExpr i ++ "]"
      showField f = "." ++ f
   in ident ++ maybe "" showIndex idx ++ maybe "" showField field

pPrintBinOp :: BinaryOp -> String
pPrintBinOp OpOr = " or "
pPrintBinOp OpAnd = " and "
pPrintBinOp OpEq = " = "
pPrintBinOp OpNeq = " != "
pPrintBinOp OpLess = " < "
pPrintBinOp OpLessEq = " <= "
pPrintBinOp OpGreater = " > "
pPrintBinOp OpGreaterEq = " >= "
pPrintBinOp OpPlus = " + "
pPrintBinOp OpMinus = " - "
pPrintBinOp OpMul = " * "
pPrintBinOp OpDiv = " / "

pPrintUnOp :: UnaryOp -> String
pPrintUnOp OpNot = "not "
pPrintUnOp OpNeg = "-"

pPrintExpr :: Expr -> String
pPrintExpr = pPrintExpr' initialPrecedence

pPrintExpr' :: Int -> Expr -> String
pPrintExpr' _ (LVal lv) = pPrintLval lv
pPrintExpr' _ (ConstBool b) = show b
pPrintExpr' _ (ConstInt n) = show n
pPrintExpr' _ (ConstStr s) = show s
pPrintExpr' prec (UnOpExpr op expr) =
  let exprStr = pPrintExpr' (unOpPrecedence op) expr
   in pPrintUnOp op ++ exprStr
pPrintExpr' prec (BinOpExpr op lexpr rexpr) =
  let wrap parentPrec exprPrec expr
        -- 1*(2*3) is the special case of our special case: * is associative, so we can ignore
        -- the fact that it produces a different parse tree and just remove the parentheses
        | parentPrec == rhsMulPrecedence && op == OpMul = expr
        -- this expression has lower precedence than the parent expression yet comes lower in the
        -- parse tree, which means it must have been parenthesised
        | parentPrec > exprPrec = "(" ++ expr ++ ")"
        | otherwise = expr

      opPrec = binOpPrecedence op
      left = pPrintExpr' opPrec lexpr
      right = case op of
        -- if the RHS of a mul/div expression is a div expression, it was parenthesised
        -- we pretend the operator has a higher precedence to get the right parentheses
        OpDiv -> pPrintExpr' rhsDivPrecedence rexpr
        OpMul -> pPrintExpr' rhsMulPrecedence rexpr
        _ -> pPrintExpr' opPrec rexpr
   in wrap prec opPrec $ left ++ pPrintBinOp op ++ right
