-- Stewart Webb - sjwebb@student.unimelb.edu.au
-- Ben Frengley - bfrengley@student.unimelb.edu.au
-- this is needed to access __FILE__ macro
{-# LANGUAGE CPP #-}
module Roo.SemanticTestsSpec (spec) where

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

data FixtureType = Pass | Fail

getFixturePath :: String -> String
getFixturePath fileName = toUnrootedFilePath (takeDirectory (fragment __FILE__) </> fragment "semantictests" </> fragment fileName)

filenameIsFixtureType :: FixtureType -> String -> Bool
filenameIsFixtureType Pass path =
  let pathSegments = T.splitOn (T.pack ".") (T.pack path)
   in ((length pathSegments) >= 3) && (T.pack "pass" `isInfixOf` ((reverse pathSegments) !! 1))
filenameIsFixtureType Fail path =
  let pathSegments = T.splitOn (T.pack ".") (T.pack path)
   in (length pathSegments) >= 3 && (T.pack "fail" `isInfixOf` ((reverse pathSegments) !! 1))

getFixtures :: IO [(String, FilePath, FixtureType)]
getFixtures = do
  let fixtureDirPath = toUnrootedFilePath ((takeDirectory (fragment __FILE__)) </> (fragment "semantictests"))
  dirEnts <- listDirectory fixtureDirPath
  let results = 
        foldl
          (\list ent ->
            let fullFilePath = getFixturePath ent
             in
                if isSuffixOf (T.pack "roo") (T.pack ent)
                  then
                    if filenameIsFixtureType Fail ent
                      then (ent, fullFilePath, Fail):list
                      else if filenameIsFixtureType Pass ent
                        then (ent, fullFilePath, Pass):list
                        else list
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
  forM_ fixtures (\(fixtureName, fixturePath, specType) -> makeSpec specType fixtureName fixturePath)

makeSpec :: FixtureType -> String -> FilePath -> SpecWith ()
makeSpec Pass fixtureName fixturePath = describe fixtureName $ do
  it "compiles with success" $ do
    testSourceCode <- readFile fixturePath
    let parseResult = parseRooProgram fixturePath testSourceCode
    parseResult `shouldSatisfy` isRight
    let ast = unwrapRight parseResult
        compileResult = compileRooProgram ast
    compileResult `shouldSatisfy` isRight

makeSpec Fail fixtureName fixturePath = describe fixtureName $ do
  it "parses successfully" $ do
    testSourceCode <- readFile fixturePath
    let parseResult = parseRooProgram fixturePath testSourceCode
    parseResult `shouldSatisfy` isRight
  it "fails compilation" $ do
    testSourceCode <- readFile fixturePath
    let parseResult = parseRooProgram fixturePath testSourceCode
    parseResult `shouldSatisfy` isRight
    let ast = unwrapRight parseResult
        compileResult = compileRooProgram ast
    compileResult `shouldSatisfy` isLeft

