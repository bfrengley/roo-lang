-- Stewart Webb - sjwebb@student.unimelb.edu.au
-- Ben Frengley - bfrengley@student.unimelb.edu.au
-- this is needed to access __FILE__ macro
{-# LANGUAGE CPP #-}
module Roo.CompileAllExamplesSpec (spec) where

import Control.Monad
import qualified Data.Text as T
import Data.Either (isRight, isLeft)
import Data.Text ( isSuffixOf, isInfixOf )
import System.Directory ( listDirectory )
import System.Path

import Test.Hspec

import Parser (parseRooProgram)
import CodeGen (compileRooProgram)

import Debug.Trace

examplesDir = takeDirectory (fragment __FILE__) </> fragment ".." </> fragment ".." </> fragment "examples"

getFixturePath :: String -> String
getFixturePath fileName = toUnrootedFilePath (examplesDir </> fragment fileName)

fixtureNameEndsWith :: String -> String -> Bool
fixtureNameEndsWith fixtureName suffix = isSuffixOf (T.pack suffix) (T.pack fixtureName)

getFixtures :: IO [(String, FilePath)]
getFixtures = do
  let fixtureDirPath = toUnrootedFilePath (examplesDir)
  dirEnts <- listDirectory fixtureDirPath
  let results = 
        foldl
          (\list ent ->
            let fullFilePath = getFixturePath ent
             in if ent `fixtureNameEndsWith` ".roo"
                  then (ent, fullFilePath):list
                  else list
          )
          []
          dirEnts
  return results

unwrapRight :: Either a b -> b
unwrapRight (Right x) = x
unwrapRight (Left x) = error "unwrapped a Left when expecting a Right!"

spec :: Spec
spec = do
  fixtures <- runIO getFixtures
  forM_ fixtures (\(fixtureName, fixturePath) -> makeSpec fixtureName fixturePath)

makeSpec :: String -> FilePath -> SpecWith ()
makeSpec fixtureName fixturePath = describe fixtureName $ do
  it "parses successfully" $ do
    testSourceCode <- readFile fixturePath
    let parseResult = parseRooProgram fixturePath testSourceCode
    parseResult `shouldSatisfy` isRight
  unless
    (fixtureName `fixtureNameEndsWith` ".parse.roo")
    (
      if (fixtureName `fixtureNameEndsWith` ".fail.roo")
      then
        (
          it "compiles with failure" $ do
            testSourceCode <- readFile fixturePath
            let parseResult = parseRooProgram fixturePath testSourceCode
            parseResult `shouldSatisfy` isRight
            let ast = unwrapRight parseResult
                compileResult = compileRooProgram ast
            compileResult `shouldSatisfy` isLeft
        )
      else
        (
          it "compiles with success" $ do
            testSourceCode <- readFile fixturePath
            let parseResult = parseRooProgram fixturePath testSourceCode
            parseResult `shouldSatisfy` isRight
            let ast = unwrapRight parseResult
                compileResult = compileRooProgram ast
            compileResult `shouldSatisfy` isRight
        )
    )