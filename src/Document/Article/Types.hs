module Document.Article.Types where

import Data.Text (Text)
import Data.Vector (Vector)

import Document.String.Types

data Inline
  = InlineString Text
  | Bold (Vector Inline)
  deriving stock (Eq, Ord, Show)

data Block
  = Paragraph (Vector Inline)
  | Section (Vector Block)
  deriving stock (Eq, Ord, Show)

data Article
  = ALiteral (Vector Block)
  | AExpr StringExpr
  deriving stock (Eq, Ord, Show)
