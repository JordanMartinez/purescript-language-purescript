module Language.PureScript.AST.Operators where

import Prelude

import Codec.Json.Unidirectional.Value (DecodeError(..), fromRecordN, fromRequired, toInt, toRecordN, toRequired, toString)
import Codec.Json.Unidirectional.Value as Json
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

-- |
-- A precedence level for an infix operator
--
type Precedence = Int

-- |
-- Associativity for infix operators
--
data Associativity
  = Infixl
  | Infixr
  | Infix

derive instance Eq Associativity
derive instance Ord Associativity
derive instance Generic Associativity _
instance Show Associativity where
  show x = genericShow x

fromAssociativity :: Associativity -> Json
fromAssociativity = Json.fromString <<< case _ of
  Infixl -> "infixl"
  Infixr -> "infixr"
  Infix -> "infix"

toAssociativity :: Json -> Either Json.DecodeError Associativity
toAssociativity = toString >=> case _ of
  "infixl" -> pure Infixl
  "infixr" -> pure Infixr
  "infix" -> pure Infix
  str -> Left $ DecodeError $ "Expected 'infixl', 'infixr', or 'infix' but got '" <> str <> "'"

-- |
-- Fixity data for infix operators
--
newtype Fixity = Fixity
  { associativity :: Associativity
  , precedence :: Precedence
  }

derive instance Eq Fixity
derive instance Ord Fixity
derive instance Newtype Fixity _
derive instance Generic Fixity _
instance Show Fixity where
  show x = genericShow x

fromFixity :: Fixity -> Json
fromFixity = fromRecordN Fixity
  { associativity: fromRequired fromAssociativity
  , precedence: fromRequired Json.fromInt
  }

toFixity :: Json -> Either Json.DecodeError Fixity
toFixity = toRecordN Fixity
  { associativity: toRequired toAssociativity
  , precedence: toRequired toInt
  }
