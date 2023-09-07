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

associativityJSON :: Associativity -> Json
associativityJSON = Json.fromString <<< case _ of
  Infixl -> "infixl"
  Infixr -> "infixr"
  Infix -> "infix"

jsonAssociativity :: Json -> Either Json.DecodeError Associativity
jsonAssociativity = toString >=> case _ of
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

fixityJSON :: Fixity -> Json
fixityJSON = fromRecordN Fixity
  { associativity: fromRequired associativityJSON
  , precedence: fromRequired Json.fromInt
  }

jsonFixity :: Json -> Either Json.DecodeError Fixity
jsonFixity = toRecordN Fixity
  { associativity: toRequired jsonAssociativity
  , precedence: toRequired toInt
  }
