-- Stewart Webb - sjwebb@student.unimelb.edu.au
-- Ben Frengley - bfrengley@student.unimelb.edu.au
-- this is needed to access __FILE__ macro
{-# LANGUAGE CPP #-}

module OzWriter.OzWriterSpec (spec) where

import qualified Data.Text as T
import OzAST
import OzWriter
import System.Path
import Test.Hspec

getFixturePath :: String -> String
getFixturePath fileName = toUnrootedFilePath (takeDirectory (fragment __FILE__) </> fragment fileName)

getFixture :: String -> IO String
getFixture fixtureName = readFile (getFixturePath fixtureName)

spec :: Spec
spec = do
  describe "renderProgram" $ do
    it "correctly prints simpletest" $ do
      referenceCode <- getFixture "simpletest.oz"
      writeProgram simpleTestAst `shouldBe` T.pack referenceCode
  where
    -- prettyPrintTestCode = "blah blah blah blah"
    simpleTestAst =
      Program
        [ InstrCall (Label "proc_main"),
          InstrHalt
        ]
        [ LabelledBlock
            (Label "proc_main")
            [ InstrPushStackFrame (Framesize 3),
              InstrIntConst (Register 0) (IntegerConst 0),
              InstrStore (StackSlot 0) (Register 0),
              InstrIntConst (Register 0) (IntegerConst 0),
              InstrStore (StackSlot 1) (Register 0),
              InstrCallBuiltin BuiltinReadInt,
              InstrLoadAddress (Register 1) (StackSlot 0),
              InstrStoreIndirect (Register 1) (Register 0),
              InstrCallBuiltin BuiltinReadInt,
              InstrLoadAddress (Register 1) (StackSlot 1),
              InstrStoreIndirect (Register 1) (Register 0),
              InstrLoad (Register 1) (StackSlot 0),
              InstrLoad (Register 2) (StackSlot 1),
              InstrMulInt (Register 0) (Register 1) (Register 2),
              InstrCallBuiltin BuiltinPrintInt,
              InstrPopStackFrame (Framesize 3),
              InstrReturn
            ]
        ]
