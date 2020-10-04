-- |
-- Module: OzAST
-- Description: Data types which encode the abstract syntax tree of a valid Oz program.
-- Maintainer: Stewart Webb <sjwebb@student.unimelb.edu.au>
--             Ben Frengley <bfrengley@student.unimelb.edu.au>
--
-- An Oz program consists of a sequence of instructions, followed by an
-- optional list of labelled blocks of other instructions representing named
-- chunks of code that any other code can 'jump' to.

module OzAST where

import Control.Monad

-- Argument types

data Framesize
  = Framesize Word
data StackSlot
  = StackSlot Word
data Register
  = Register Word
data IntegerConst
  = IntegerConst Integer
data StringConst
  = StringConst String

data Label = Label String

data Program = Program [Instruction] [LabelledBlock]
  -- deriving (Eq, Show)

data LabelledBlock = LabelledBlock Label [Instruction]

data Instruction
  = InstrPushStackFrame Framesize
  | InstrPopStackFrame Framesize
  |
    -- | Store into the memory location at stack slot S the value in register R
    -- C equivalent:
    -- > stackVar = regVar
    InstrStore StackSlot Register
  |
    -- | Load into register R the value in stack slot S's memory location 
    -- C equivalent:
    -- > regVar = stackVar
    InstrLoad Register StackSlot
  |
    -- | Load into register R the address of the memory location of stack slot S
    -- C equivalent:
    -- > regVar = &stackVar
    InstrLoadAddress Register StackSlot
  |
    -- | Load into register R1 the value of the memory cell located in the
    -- memory cell number corresponding to the numeric value stored in
    -- register R2
    -- C equivalent:
    -- > regVar1 = *regVar2
    InstrLoadIndirect Register Register
  |
    -- | Store into the memory cell located in the memory cell number corresponding
    -- to the numeric value stored in register R1 the value of register R2
    -- C equivalent:
    -- > *regVar1 = regVar2
    InstrStoreIndirect Register Register
  | InstrIntConst Register IntegerConst
  -- InstrRealConst
  | InstrStringConst Register StringConst
  | InstrAddInt Register Register Register
  -- InstrAddReal
  | InstrAddOffset Register Register Register
  | InstrSubInt Register Register Register
  -- InstrSubReal
  | InstrSubOffset Register Register Register
  | InstrMulInt Register Register Register
  -- InstrMulReal
  | InstrDivInt Register Register Register
  -- InstrDivReal
  | InstrNegInt Register Register
  -- InstrNegReal
  | InstrCmpEqualInt Register Register Register
  | InstrCmpNotEqualInt Register Register Register
  | InstrCmpGreaterThanInt Register Register Register
  | InstrCmpGreaterEqualInt Register Register Register
  | InstrCmpLessThanInt Register Register Register
  | InstrCmpLessEqualInt Register Register Register
  | InstrAnd Register Register Register
  | InstrOr Register Register Register
  | InstrNot Register Register
  -- InstrIntToReal
  | InstrMove Register Register
  | InstrBranchOnTrue Register Label
  | InstrBranchOnFalse Register Label
  | InstrBranchUnconditional Label
  | InstrCall Label
  | InstrCallBuiltin BuiltinFunction
  | InstrReturn
  | InstrHalt
  | InstrDebugRegister Register
  | InstrDebugSlot StackSlot
  | InstrDebugStack

data BuiltinFunction
  = BuiltinReadInt
  -- BuiltinReadReal
  | BuiltinReadBool
  | BuiltinPrintInt
  -- BuiltinPrintReal
  | BuiltinPrintBool
  | BuiltinPrintString