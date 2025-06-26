module TestUtils where

import Data.Function
import Effectful
import Effectful.Fail (Fail)
import Effectful.Fail qualified as Fail
import Effectful.State.Static.Local (State)
import Effectful.State.Static.Local qualified as State
import GHC.Stack
import Test.Tasty (TestTree)
import Test.Tasty qualified as Test
import Test.Tasty.HUnit qualified as Test

import Document.String.Expression

type TestEff = Eff '[State Env, Fail, IOE]

runTestEff
  :: Eff [State Env, Fail, IOE] a
  -> IO a
runTestEff action = do
  action
    & State.evalState emptyEnv
    & Fail.runFailIO
    & runEff

assertFailure :: HasCallStack => MonadIO m => String -> m ()
assertFailure = liftIO . Test.assertFailure

assertRight :: HasCallStack => Either a b -> TestEff b
assertRight (Left _a) = liftIO $ Test.assertFailure "Test return Left instead of Right"
assertRight (Right b) = pure b

testThis :: String -> TestEff () -> TestEff TestTree
testThis name assertion = do
  let test = runTestEff assertion
  pure $
    Test.testCase name test

testThese :: String -> [TestEff TestTree] -> TestEff TestTree
testThese groupName tests = fmap (Test.testGroup groupName) newTests
  where
    newTests :: TestEff [TestTree]
    newTests = sequenceA tests

assertBool :: HasCallStack => Bool -> TestEff ()
assertBool boolean = liftIO $ Test.assertBool "" boolean

assertEqual :: (Eq a, HasCallStack, Show a) => a -> a -> TestEff ()
assertEqual actual expected = liftIO $ Test.assertEqual "" actual expected
