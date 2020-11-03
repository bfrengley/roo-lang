-- |
-- Module: OzAST
-- Description: Data types which encode the abstract syntax tree of a valid Oz program.
-- Maintainer: Stewart Webb <sjwebb@student.unimelb.edu.au>
--             Ben Frengley <bfrengley@student.unimelb.edu.au>
--
-- An Oz program consists of a sequence of instructions and or labels, which
-- are points that can be jumped to by control flow instructions.
--
-- The Roo compiler converts a Roo AST into an Oz AST before serializing it; this module
-- defines the  AST types for Oz (and how some of the basic types are printed).
module OzAST where

-- | Typeclass for representing types that have a particular string
--   representation in an Oz program. This acts as "one standard way"
--   for getting the correct Oz representations for basic OzAST types.
class OzShow a where
  ozShow :: a -> String

-- Argument types
-----------------

newtype Framesize = Framesize Int deriving (Show, Eq)

instance OzShow Framesize where
  ozShow (Framesize w) = show w

newtype StackSlot = StackSlot Int deriving (Show, Eq)

instance OzShow StackSlot where
  ozShow (StackSlot w) = show w

newtype Register = Register Int deriving (Show, Eq)

instance OzShow Register where
  ozShow (Register w) = "r" ++ show w

newtype IntegerConst = IntegerConst Integer deriving (Show, Eq)

instance OzShow IntegerConst where
  ozShow (IntegerConst i) = show i

newtype StringConst = StringConst String deriving (Show, Eq)

instance OzShow StringConst where
  ozShow (StringConst s) = "\"" ++ s ++ "\""

-- Program element types
------------------------

newtype Label = Label String deriving (Show, Eq)

instance OzShow Label where
  ozShow (Label s) = s

data Program = Program [ProgramLine] deriving (Show)

data ProgramLine = InstructionLine Instruction | LabelLine Label deriving (Show)

-- | Type representing all possible Oz instruction types
data Instruction
  =
    -- Stack instructions
    ---------------------

    InstrPushStackFrame Framesize
  | InstrPopStackFrame Framesize
    -- | Store into the memory location at stack slot S the value in register R.
    -- C equivalent:
    -- > stackVar = regVar
  | InstrStore StackSlot Register
    -- | Load into register R the value in stack slot S's memory location.
    -- C equivalent:
    -- > regVar = stackVar
  | InstrLoad Register StackSlot
    -- | Load into register R the address of the memory location of stack slot S.
    -- C equivalent:
    -- > regVar = &stackVar
  | InstrLoadAddress Register StackSlot
    -- | Load into register R1 the value of the memory cell located in the
    -- memory cell number corresponding to the numeric value stored in
    -- register R2.
    -- C equivalent:
    -- > regVar1 = *regVar2
  | InstrLoadIndirect Register Register
    -- | Store into the memory cell located in the memory cell number corresponding
    -- to the numeric value stored in register R1 the value of register R2
    -- C equivalent:
    -- > *regVar1 = regVar2
  | InstrStoreIndirect Register Register

    -- Constant value instructions
    ------------------------------

  | InstrIntConst Register IntegerConst
    -- InstrRealConst
  | InstrStringConst Register StringConst

    -- Arithmetic instructions
    --------------------------

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

    -- Comparison instructions
    -- -----------------------

  | InstrCmpEqualInt Register Register Register
  | InstrCmpNotEqualInt Register Register Register
  | InstrCmpGreaterThanInt Register Register Register
  | InstrCmpGreaterEqualInt Register Register Register
  | InstrCmpLessThanInt Register Register Register
  | InstrCmpLessEqualInt Register Register Register

    -- Boolean operators
  | InstrAnd Register Register Register
  | InstrOr Register Register Register
  | InstrNot Register Register

    -- InstrIntToReal

    -- | Copy the value in one register into another
  | InstrMove Register Register

    -- Control flow instructions
    ----------------------------

  | InstrBranchOnTrue Register Label
  | InstrBranchOnFalse Register Label
  | InstrBranchUnconditional Label

  -- Function/Procedure instructions
  ----------------------------------

  | InstrCall Label
  | InstrCallBuiltin BuiltinFunction
  | InstrReturn

    -- | Stop program execution
  | InstrHalt
  
  -- Debugging instructions
  -------------------------

  | InstrDebugRegister Register
  | InstrDebugSlot StackSlot
  | InstrDebugStack
  deriving (Show)

data BuiltinFunction
  = BuiltinReadInt
  | -- BuiltinReadReal
    BuiltinReadBool
  | BuiltinPrintInt
  | -- BuiltinPrintReal
    BuiltinPrintBool
  | BuiltinPrintString
  deriving (Show)

-- | Mapping from Haskell types to all the Oz string names of the builtin functions
instance OzShow BuiltinFunction where
  ozShow BuiltinReadInt = "read_int"
  ozShow BuiltinReadBool = "read_bool"
  ozShow BuiltinPrintInt = "print_int"
  ozShow BuiltinPrintBool = "print_bool"
  ozShow BuiltinPrintString = "print_string"

-- | Utility function for defining a boolean constant safely
boolConst :: Bool -> IntegerConst
-- Spec says any non-zero int is 'true' - just use 1
boolConst True = IntegerConst 1
boolConst False = IntegerConst 0
