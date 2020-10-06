-- Stewart Webb - sjwebb@student.unimelb.edu.au
-- Ben Frengley - bfrengley@student.unimelb.edu.au
-- this is needed to access __FILE__ macro
{-# LANGUAGE CPP #-}

module PrettyPrint.PrettyPrintSpec (spec) where

import AST
import Data.Text
import PrettyPrint
import System.Path
import Test.Hspec
import Text.Parsec.Pos (initialPos)

getFixturePath :: String -> String
getFixturePath fileName = toUnrootedFilePath (takeDirectory (fragment __FILE__) </> fragment fileName)

getFixture :: String -> IO String
getFixture fixtureName = readFile (getFixturePath fixtureName)

spec :: Spec
spec = do
  describe "prettyPrint" $ do
    it "pretty-prints simpleTest1" $ do
      prettyPrintTestCode <- getFixture "simpleTest1.roo"
      prettyPrint simpleText1Ast `shouldBe` Data.Text.pack prettyPrintTestCode
  where
    -- prettyPrintTestCode = "blah blah blah blah"
    simpleText1Ast =
      Program
        []
        []
        [ Procedure
            ( ProcHead
                (initialPos "")
                (Ident (initialPos "") "main")
                [ ProcParam (initialPos "") (ParamBuiltinT TBool PassByRef) (Ident (initialPos "") "arg1")
                ]
            )
            ( ProcBody
                [ VarDecl (initialPos "") (VarBuiltinT TBool) [Ident (initialPos "") "myBool"]
                ]
                [ SAtom (initialPos "") (Write (ConstStr (initialPos "") "Hello World!"))
                ]
            )
        ]
