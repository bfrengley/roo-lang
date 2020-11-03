{-# LANGUAGE OverloadedStrings #-}

module OzWriter where

import qualified Data.Text as T
import OzAST
import PrettyPrint (indent)

writeProgram :: Program -> T.Text
writeProgram (Program programLines) =
  T.unlines (map writeProgramLine programLines)

writeProgramLine :: ProgramLine -> T.Text
writeProgramLine (InstructionLine instr) = indent $ writeInstruction instr
writeProgramLine (LabelLine label) = writeLabel label

writeLabel :: Label -> T.Text
writeLabel label = ozShowT label <> ":"

writeInstruction :: Instruction -> T.Text
writeInstruction (InstrPushStackFrame fs) = writeInstrWithArgs "push_stack_frame" [fs]
writeInstruction (InstrPopStackFrame fs) = writeInstrWithArgs "pop_stack_frame" [fs]
writeInstruction (InstrStore ss r) = writeInstrWithArgs "store" [ozShowT ss, ozShowT r]
writeInstruction (InstrLoad r ss) = writeInstrWithArgs "load" [ozShowT r, ozShowT ss]
writeInstruction (InstrLoadAddress r ss) = writeInstrWithArgs "load_address" [ozShowT r, ozShowT ss]
writeInstruction (InstrLoadIndirect rDest rSrc) = writeInstrWithArgs "load_indirect" [rDest, rSrc]
writeInstruction (InstrStoreIndirect rDest rSrc) = writeInstrWithArgs "store_indirect" [rDest, rSrc]
writeInstruction (InstrIntConst rDest int) = writeInstrWithArgs "int_const" [ozShowT rDest, ozShowT int]
writeInstruction (InstrStringConst rDest str) = writeInstrWithArgs "string_const" [ozShowT rDest, ozShowT str]
writeInstruction (InstrAddInt rDest rLeft rRight) = writeInstrWithArgs "add_int" [rDest, rLeft, rRight]
writeInstruction (InstrAddOffset rDest rLeft rRight) = writeInstrWithArgs "add_offset" [rDest, rLeft, rRight]
writeInstruction (InstrSubInt rDest rLeft rRight) = writeInstrWithArgs "sub_int" [rDest, rLeft, rRight]
writeInstruction (InstrSubOffset rDest rLeft rRight) = writeInstrWithArgs "sub_offset" [rDest, rLeft, rRight]
writeInstruction (InstrMulInt rDest rLeft rRight) = writeInstrWithArgs "mul_int" [rDest, rLeft, rRight]
writeInstruction (InstrDivInt rDest rLeft rRight) = writeInstrWithArgs "div_int" [rDest, rLeft, rRight]
writeInstruction (InstrNegInt rDest rSrc) = writeInstrWithArgs "neg_int" [rDest, rSrc]
writeInstruction (InstrCmpEqualInt rDest rLeft rRight) = writeInstrWithArgs "cmp_eq_int" [rDest, rLeft, rRight]
writeInstruction (InstrCmpNotEqualInt rDest rLeft rRight) = writeInstrWithArgs "cmp_ne_int" [rDest, rLeft, rRight]
writeInstruction (InstrCmpGreaterThanInt rDest rLeft rRight) = writeInstrWithArgs "cmp_gt_int" [rDest, rLeft, rRight]
writeInstruction (InstrCmpGreaterEqualInt rDest rLeft rRight) = writeInstrWithArgs "cmp_ge_int" [rDest, rLeft, rRight]
writeInstruction (InstrCmpLessThanInt rDest rLeft rRight) = writeInstrWithArgs "cmp_lt_int" [rDest, rLeft, rRight]
writeInstruction (InstrCmpLessEqualInt rDest rLeft rRight) = writeInstrWithArgs "cmp_le_int" [rDest, rLeft, rRight]
writeInstruction (InstrAnd rDest rLeft rRight) = writeInstrWithArgs "and" [rDest, rLeft, rRight]
writeInstruction (InstrOr rDest rLeft rRight) = writeInstrWithArgs "or" [rDest, rLeft, rRight]
writeInstruction (InstrNot rDest rSrc) = writeInstrWithArgs "not" [rDest, rSrc]
writeInstruction (InstrMove rDest rSrc) = writeInstrWithArgs "move" [rDest, rSrc]
writeInstruction (InstrBranchOnTrue r l) = writeInstrWithArgs "branch_on_true" [ozShowT r, ozShowT l]
writeInstruction (InstrBranchOnFalse r l) = writeInstrWithArgs "branch_on_false" [ozShowT r, ozShowT l]
writeInstruction (InstrBranchUnconditional l) = writeInstrWithArgs "branch_uncond" [ozShowT l]
writeInstruction (InstrCall l) = writeInstrWithArgs "call" [ozShowT l]
writeInstruction (InstrCallBuiltin bif) = writeInstrWithArgs "call_builtin" [bif]
writeInstruction (InstrReturn) = "return"
writeInstruction (InstrHalt) = "halt"
writeInstruction (InstrDebugRegister r) = writeInstrWithArgs "debug_reg" [ozShowT r]
writeInstruction (InstrDebugSlot ssn) = writeInstrWithArgs "debug_slot" [ozShowT ssn]
writeInstruction (InstrDebugStack) = "debug_stack"

writeInstrWithArgs :: OzShow s => T.Text -> [s] -> T.Text
writeInstrWithArgs instr args = T.unwords [instr, T.intercalate ", " $ map ozShowT args]

ozShowT :: OzShow s => s -> T.Text
ozShowT = T.pack . ozShow

instance OzShow T.Text where
  ozShow = T.unpack
