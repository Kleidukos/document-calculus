module Document.String.Expression where

import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Fail
import Effectful.State.Static.Local (State)
import Effectful.State.Static.Local qualified as State

data StringExpr
  = SLiteral Text
  | Concat StringExpr StringExpr
  | Let
      Text
      -- ^ Binding
      StringExpr
      -- ^ Expression
      StringExpr
      -- ^ Body
  | Var Text
  | StringTemplate Template
  deriving stock (Eq, Ord, Show)

newtype Template = Template {getTemplateParts :: Vector TemplatePart}
  deriving newtype (Eq, Ord, Show)

data TemplatePart
  = TemplateString Text
  | InterpolateExpression StringExpr
  | TemplateSet Text StringExpr
  deriving stock (Eq, Ord, Show)

data Env = Env
  { bindings :: Map Text StringExpr
  }
  deriving stock (Eq, Ord, Show)

emptyEnv :: Env
emptyEnv = Env mempty

addBinding :: State Env :> es => Text -> StringExpr -> Eff es ()
addBinding name expression =
  State.modify $ \env ->
    let newBindings = Map.insert name expression env.bindings
     in env{bindings = newBindings}

lookupBinding :: State Env :> es => Text -> Eff es StringExpr
lookupBinding name = do
  Env{bindings} <- State.get
  case Map.lookup name bindings of
    Nothing -> do
      error $ "Could not find bindings " <> Text.unpack name <> "\nAvailable bindings: " <> show bindings
    Just value -> pure value

evalExpr :: (Fail :> es, State Env :> es) => StringExpr -> Eff es StringExpr
evalExpr (SLiteral t) = pure $ SLiteral t
evalExpr (Concat s1 s2) = do
  SLiteral evaluated1 <- evalExpr s1
  SLiteral evaluated2 <- evalExpr s2
  pure $ SLiteral (evaluated1 <> evaluated2)
evalExpr (Let name expression body) = do
  addBinding name expression
  evalExpr body
evalExpr (Var name) =
  lookupBinding name
evalExpr (StringTemplate _) = error "Don't evaluate templates, idiot."

desugar :: StringExpr -> Eff es StringExpr
desugar (SLiteral t) = pure $ SLiteral t
desugar (Concat t1 t2) = Concat <$> desugar t1 <*> desugar t2
desugar (Let name body expression) = Let name <$> desugar body <*> desugar expression
desugar (Var name) = pure $ Var name
desugar (StringTemplate t) = do
  result <- desugarTemplate t
  pure $ Vector.foldr1 (Concat) result

desugarTemplate :: Template -> Eff es (Vector StringExpr)
desugarTemplate (Template template) = case Vector.uncons template of
  Nothing -> pure Vector.empty
  Just (p, ps) -> do
    case p of
      TemplateSet var body -> do
        desugaredVector <- desugarTemplate (Template ps)
        let desugaredTemplate = Vector.foldr1 (Concat) desugaredVector
        pure $
          Vector.singleton $
            Let
              var
              body
              desugaredTemplate
      _ -> do
        desugaredP <- desugarTemplatePart p
        desugaredPS <- desugarTemplate (Template ps)
        pure $
          Vector.cons
            desugaredP
            desugaredPS

desugarTemplatePart :: TemplatePart -> Eff es StringExpr
desugarTemplatePart (TemplateString s) = pure (SLiteral s)
desugarTemplatePart (InterpolateExpression e) = desugar e
desugarTemplatePart (TemplateSet _ _) = undefined
