module Document.String.Types where

import Data.Text (Text)
import Data.IntMap (IntMap)

type VarPos = IntMap Int

newtype Closure a = Closure {callClosure ::VarPos -> Env -> a}

data Boxed a 
  = Box a
  | Env Int (Closure a)

data Var a = Var
  { key :: Int
  , name :: Text
  , buildFree :: Var a -> a
  }

data Binder

data ProgramExpression = 
  ELit Text
  | EConcat ProgramExpression ProgramExpression
  | ELambda ProgramExpression ProgramExpression
  | Let (ProgramExpression, ProgramExpression) 
