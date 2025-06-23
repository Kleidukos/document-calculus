module Document.String.Types where

import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Effectful
import Effectful.Fail
import Effectful.State.Static.Local (State)
import Effectful.State.Static.Local qualified as State
import qualified Data.Vector as Vector

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
  | StringTemplate (Vector StringExpr)
  deriving stock (Eq, Ord, Show)

data TemplatePart 
  = TemplateLiteral Text
  | TemplateExpr StringExpr
  deriving stock (Eq, Ord, Show)

data Env = Env
  { bindings :: Map Text StringExpr
  }
  deriving stock (Eq, Ord, Show)

emptyEnv :: Env
emptyEnv = Env mempty

addBinding :: State Env :> es => Text -> StringExpr -> Eff es ()
addBinding name expression = State.modify $ \env ->
  let newBindings = Map.insert name expression env.bindings
   in env{bindings = newBindings}

lookupBinding :: State Env :> es => Text -> Eff es StringExpr
lookupBinding name = do
  Env{bindings} <- State.get
  case Map.lookup name bindings of
    Nothing -> error $ "Could not find bindings " <> Text.unpack name
    Just value -> pure value

data StringType
  = StringType
  deriving stock (Eq, Ord, Show)

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
evalExpr (StringTemplate t) = undefined

desugarTemplate :: Vector TemplatePart -> Vector StringExpr
desugarTemplate template  = case Vector.uncons template of
  Nothing ->  Vector.empty
  Just (p, ps) -> 
    
