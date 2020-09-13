-- Stewart Webb - sjwebb@student.unimelb.edu.au
-- Ben Frengley - bfrengley@student.unimelb.edu.au

-- this is needed to access __FILE__ macro
{-# LANGUAGE CPP #-}
module PrettyPrint.PrettyPrintSpec (spec) where

-- import System.Environment
import System.Path
-- import Sy    stem.FilePath

import Test.Hspec

import AST
import PrettyPrint

getFixturePath :: String -> String
getFixturePath fileName = toUnrootedFilePath ((takeDirectory (fragment __FILE__)) </> fragment fileName)

getFixture :: String -> IO String
getFixture fixtureName = readFile (getFixturePath fixtureName)

spec :: Spec
spec = do
  describe "prettyPrint" $ do
    it "pretty-prints simpleTest1" $ do
      prettyPrintTestCode <- getFixture "simpleTest1.roo"
      prettyPrint simpleText1Ast `shouldBe` prettyPrintTestCode
        where
            -- prettyPrintTestCode = "blah blah blah blah"
            simpleText1Ast =
                Program
                    []
                    []
                    [
                        Procedure
                            (
                                ProcHead
                                    "main"
                                    [
                                        ProcParam (BoolParam PassByVal) "arg1"
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
