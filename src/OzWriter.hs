module OzWriter where

import Data.List (intercalate)

import OzAST

renderProgram :: Program -> String
renderProgram (Program instrs blocks) =
    let
        renderInstructionLine = ("    "++) . renderInstruction
        renderLabelledBlockLines = \(LabelledBlock (Label label) blockInstrs) ->
                let
                    blockLines = map renderInstructionLine blockInstrs
                in
                    label ++ ":" ++ "\n" ++ (intercalate "\n" blockLines)
        lines = (map renderInstructionLine instrs)
                ++
                (map renderLabelledBlockLines blocks)
    in
        intercalate "\n" lines

renderInstruction :: Instruction -> String

renderInstruction (InstrPushStackFrame (Framesize fs)) = renderInstrWithArgs "push_stack_frame" [(show fs)]
renderInstruction (InstrPopStackFrame (Framesize fs)) = renderInstrWithArgs "pop_stack_frame" [(show fs)]
renderInstruction (InstrStore (StackSlot ss) r) = renderInstrWithArgs "store" [show ss, renderRegister r]
renderInstruction (InstrLoad r (StackSlot ss)) = renderInstrWithArgs "load" [renderRegister r, show ss]
renderInstruction (InstrLoadAddress r (StackSlot ss)) = renderInstrWithArgs "load_address" [renderRegister r, show ss]
renderInstruction (InstrLoadIndirect rDest rSrc) = renderInstrWithArgs  "load_indirect" (renderRegister `map` [rDest, rSrc])
renderInstruction (InstrStoreIndirect rDest rSrc) = renderInstrWithArgs "store_indirect" (renderRegister `map` [rDest, rSrc])
renderInstruction (InstrIntConst rDest (IntegerConst int)) = renderInstrWithArgs "int_const" [renderRegister rDest, show int]
renderInstruction (InstrStringConst rDest (StringConst str)) = renderInstrWithArgs "string_const" [renderRegister rDest, show str]
renderInstruction (InstrAddInt rDest rLeft rRight) = renderInstrWithArgs "add_int" (renderRegister `map` [rDest, rLeft, rRight])
renderInstruction (InstrAddOffset rDest rLeft rRight) = renderInstrWithArgs "add_offset" (renderRegister `map` [rDest, rLeft, rRight])
renderInstruction (InstrSubInt rDest rLeft rRight) = renderInstrWithArgs "sub_int" (renderRegister `map` [rDest, rLeft, rRight])
renderInstruction (InstrSubOffset rDest rLeft rRight) = renderInstrWithArgs "sub_offset" (renderRegister `map` [rDest, rLeft, rRight])
renderInstruction (InstrMulInt rDest rLeft rRight) = renderInstrWithArgs "mul_int" (renderRegister `map` [rDest, rLeft, rRight])
renderInstruction (InstrDivInt rDest rLeft rRight) = renderInstrWithArgs "div_int" (renderRegister `map` [rDest, rLeft, rRight])
renderInstruction (InstrNegInt rDest rSrc) = renderInstrWithArgs "neg_int" (renderRegister `map` [rDest, rSrc])
renderInstruction (InstrCmpEqualInt rDest rLeft rRight) = renderInstrWithArgs "cmp_eq_int" (renderRegister `map` [rDest, rLeft, rRight])
renderInstruction (InstrCmpNotEqualInt rDest rLeft rRight) = renderInstrWithArgs "cmp_ne_int" (renderRegister `map` [rDest, rLeft, rRight])
renderInstruction (InstrCmpGreaterThanInt rDest rLeft rRight) = renderInstrWithArgs "cmp_gt_int" (renderRegister `map` [rDest, rLeft, rRight])
renderInstruction (InstrCmpGreaterEqualInt rDest rLeft rRight) = renderInstrWithArgs "cmp_ge_int" (renderRegister `map` [rDest, rLeft, rRight])
renderInstruction (InstrCmpLessThanInt rDest rLeft rRight) = renderInstrWithArgs "cmp_lt_int" (renderRegister `map` [rDest, rLeft, rRight])
renderInstruction (InstrCmpLessEqualInt rDest rLeft rRight) = renderInstrWithArgs "cmp_le_int" (renderRegister `map` [rDest, rLeft, rRight])
renderInstruction (InstrAnd rDest rLeft rRight) = renderInstrWithArgs "and" (renderRegister `map` [rDest, rLeft, rRight])
renderInstruction (InstrOr rDest rLeft rRight) = renderInstrWithArgs "or" (renderRegister `map` [rDest, rLeft, rRight])
renderInstruction (InstrNot rDest rSrc) = renderInstrWithArgs "not" (renderRegister `map` [rDest, rSrc])
renderInstruction (InstrMove rDest rSrc) = renderInstrWithArgs "move" (renderRegister `map` [rDest, rSrc])
renderInstruction (InstrBranchOnTrue r (Label l)) = renderInstrWithArgs "branch_on_true" [renderRegister r, l]
renderInstruction (InstrBranchOnFalse r (Label l)) = renderInstrWithArgs "branch_on_false" [renderRegister r, l]
renderInstruction (InstrBranchUnconditional (Label l)) = renderInstrWithArgs "branch_uncond" [l]
renderInstruction (InstrCall (Label l)) = renderInstrWithArgs "call" [l]
renderInstruction (InstrCallBuiltin bif) = renderInstrWithArgs "call_builtin" [renderBuiltinFunction bif]
renderInstruction (InstrReturn) = "return"
renderInstruction (InstrHalt) = "halt"
renderInstruction (InstrDebugRegister r) = renderInstrWithArgs "debug_reg" [(renderRegister r)]
renderInstruction (InstrDebugSlot (StackSlot ssn)) = renderInstrWithArgs "debug_slot" [show ssn]
renderInstruction (InstrDebugStack) = "debug_stack"

renderInstrWithArgs :: String -> [String] -> String
renderInstrWithArgs instr [arg] = unwords [instr, arg]
renderInstrWithArgs instr args = unwords [instr, intercalate ", " args]

renderLabelName :: Label -> String
renderLabelName (Label labelName) = labelName

renderRegister :: Register -> String
renderRegister (Register rn) = "r" ++ show rn

renderStackSlot :: StackSlot -> String
renderStackSlot (StackSlot ss) = show ss

renderBuiltinFunction :: BuiltinFunction -> String
renderBuiltinFunction BuiltinReadInt = "read_int"
renderBuiltinFunction BuiltinReadBool = "read_real"
renderBuiltinFunction BuiltinPrintInt = "print_int"
renderBuiltinFunction BuiltinPrintBool = "print_bool"
renderBuiltinFunction BuiltinPrintString = "print_string"