module Main where

import Data.List (List)
import System.IO
import Test.Tasty

import Document.StringTest qualified as StringTest
import TestUtils

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  spec <- traverse (\comp -> runTestEff comp) specs
  defaultMain $ testGroup "Tests" spec

specs :: List (TestEff TestTree)
specs =
  [ StringTest.spec
  ]
