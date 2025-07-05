module Document.ArticleTest where

import Data.Vector qualified as Vector
import Test.Tasty

import Document.String.Expression
import Document.String.Types
import TestUtils

spec :: TestEff TestTree
spec =
  testThese
    "Evaluate String calculus"
    [ testThese
        "Article Program"
        []
    ]
