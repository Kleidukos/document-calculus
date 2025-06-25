module Document.String.Types where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Debug.Trace (traceM)
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.State.Static.Local (State)
import Effectful.State.Static.Local qualified as State

import Document.String.Expression

data StringType
  = TString
  deriving stock (Eq, Ord, Show)

data TypeCheckError
  = ExpectedLiterals
  | NoVarFound Text
  deriving stock (Eq, Ord, Show)

data TypeContextElement
  = BoundVar Text StringType
  | BoundTypeVar Text
  deriving stock (Eq, Ord, Show)

isBoundVar :: TypeContextElement -> Bool
isBoundVar (BoundVar _ _) = True
isBoundVar _ = False

boundVarEquals :: TypeContextElement -> Text -> Bool
boundVarEquals (BoundVar x _) y = x == y
boundVarEquals _ _ = False

newtype TypeContext = TypeContext {typeContext :: Vector TypeContextElement}
  deriving stock (Eq, Ord, Show)

addToTypeContext :: State TypeContext :> es => TypeContextElement -> Eff es ()
addToTypeContext element = do
  traceM $ "Adding " <> show element <> " to type context"
  State.modify $ \env ->
    let newBindings = Vector.cons element env.typeContext
     in env{typeContext = newBindings}

lookupTypeContext
  :: State TypeContext :> es
  => Text
  -> Eff es (Maybe StringType)
lookupTypeContext element = do
  TypeContext context <- State.get
  case Vector.find (\e -> e `boundVarEquals` element) context of
    Just (BoundVar _ t) -> pure $ Just t
    _ -> pure Nothing

typecheck
  :: ( Error (TypeCheckError) :> es
     , State (TypeContext) :> es
     )
  => StringExpr -> Eff es StringType
typecheck (SLiteral _) = pure TString
typecheck (Concat e1 e2) = do
  t1 <- typecheck e1
  t2 <- typecheck e2
  case (t1, t2) of
    (TString, TString) -> pure TString
    _ -> Error.throwError ExpectedLiterals
typecheck (Let name body expression) = do
  t1 <- typecheck body
  addToTypeContext (BoundVar name t1)
  typecheck expression
typecheck (Var name) = do
  result <- lookupTypeContext name
  case result of
    Just t -> pure t
    Nothing -> Error.throwError $ NoVarFound name
typecheck (StringTemplate template) = undefined

typecheckTemplate :: Template -> Eff es _
typecheckTemplate = undefined
