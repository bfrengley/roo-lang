-- this is needed to access __FILE__ macro
{-# LANGUAGE CPP #-}
module PrettyPrint.PrettyPrintSpec (spec) where

-- import System.Environment
import System.Path
-- import Sy    stem.FilePath

import Test.Hspec

import AST
import PrettyPrint

spec :: Spec
spec = do
  describe "prettyPrint" $ do
    it "prints the right thing" $ do
      putStr __FILE__
      let testSource = toUnrootedFilePath ((takeDirectory (fragment __FILE__)) </> fragment "prettyPrintTest.roo")
      putStr testSource
      prettyPrintTestCode <- readFile testSource
      prettyPrint complexAst `shouldBe` prettyPrintTestCode
        where
            -- prettyPrintTestCode = "blah blah blah blah"
            complexAst =
                Program
                    []
                    []
                    [
                        Procedure
                            (
                                ProcHead
                                    "main"
                                    [
                                        ProcParam "arg1" (BoolParam PassByVal)
                                    ]
                            )
                            (
                                ProcBody
                                    [
                                        VarDecl BoolVar ["myBool"]
                                    ]
                                    [
                                        WriteLn (ConstStr "Hello World!")
                                    ]
                            )
                    ]