module Document.StringTest where

import Document.String.Types
import Test.Tasty

import TestUtils

spec :: TestEff TestTree
spec =
  testThese
    "Evaluate String calculus"
    [ testThis "let-bindings" testLetBindings
    ] 

testLetBindings :: TestEff ()
testLetBindings = do
  let expr =  
        Let "x" (SLiteral "a")
          (Concat 
            (Var "x")
            (Concat 
                (SLiteral "b")
                (Var "x")
            ))
  result <- evalExpr expr
  assertEqual
    (SLiteral "aba")
    result
