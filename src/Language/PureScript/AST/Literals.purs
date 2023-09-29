-- | The core functional representation for literal values.
module Language.PureScript.AST.Literals where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (CodePoint)
import Data.Tuple (Tuple)
import JS.BigInt (BigInt)
import Language.PureScript.PSString (PSString)

-- | Data type for literal values. Parameterised so it can be used for Exprs and
-- Binders.
data Literal a
  -- | A numeric literal
  = NumericLiteral (Either BigInt Number)
  -- | A string literal
  | StringLiteral PSString
  -- | A character literal
  | CharLiteral CodePoint
  -- | A boolean literal
  | BooleanLiteral Boolean
  -- | An array literal
  | ArrayLiteral (Array a)
  -- | An object literal
  | ObjectLiteral (Array (Tuple PSString a))

derive instance Eq a => Eq (Literal a)
derive instance Ord a => Ord (Literal a)
derive instance Generic (Literal a) _
instance Show a => Show (Literal a) where
  show x = genericShow x

derive instance Functor Literal
