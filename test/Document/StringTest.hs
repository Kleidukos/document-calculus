module Document.StringTest where

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
        "String Program"
        [ testThis "concatenation" testConcatenation
        , testThis "let-bindings" testLetBindings
        ]
    , testThese
        "Template Desugaring"
        [ testThis "Evaluate a desugared template" testEvaluationOfDesugaredTemplate
        ]
    , testThese
        "Typecheck string programs"
        [ testThis "Typecheck concat" testConcatTypechecks
        , testThis "Typecheck template" testThatTemplateTypechecks
        ]
    ]

testLetBindings :: TestEff ()
testLetBindings = do
  let expr =
        Let
          "x"
          (SLiteral "a")
          ( Concat
              (Var "x")
              ( Concat
                  (SLiteral "b")
                  (Var "x")
              )
          )
  result <- evalExpr expr
  assertEqual
    (SLiteral "aba")
    result

testConcatenation :: TestEff ()
testConcatenation = do
  result <- evalExpr (Concat (SLiteral "hello") (SLiteral " world"))
  assertEqual
    (SLiteral "hello world")
    result

testConcatTypechecks :: TestEff ()
testConcatTypechecks = do
  let expr =
        Let
          "x"
          (SLiteral "a")
          ( Concat
              (Var "x")
              ( Concat
                  (SLiteral "b")
                  (Var "x")
              )
          )
  r <- typecheck expr
  assertEqual
    r
    TString

testThatTemplateTypechecks :: TestEff ()
testThatTemplateTypechecks = do
  let expr =
        Let
          "world"
          (SLiteral "world")
          ( StringTemplate
              ( Template
                  ( Vector.fromList
                      [ TemplateString "Hello"
                      , TemplateExpression
                          (Var "world")
                      ]
                  )
              )
          )

  r <- typecheck expr
  assertEqual
    r
    TString

testEvaluationOfDesugaredTemplate :: TestEff ()
testEvaluationOfDesugaredTemplate = do
  let template =
        Template
          ( Vector.fromList
              [ TemplateString "Hello"
              , TemplateExpression (Var "world")
              ]
          )
  let stringTemplate =
        ( Let
            "world"
            (SLiteral " World")
            (StringTemplate template)
        )

  desugaredStringTemplate <- desugar stringTemplate

  evaluatedStringTemplate <- evalExpr desugaredStringTemplate

  assertEqual
    evaluatedStringTemplate
    (SLiteral "Hello World")
