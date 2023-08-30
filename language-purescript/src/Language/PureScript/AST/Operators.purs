module Language.PureScript.AST.Operators where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import JSON (JSON)
import JSON as JSON
import JSON.ExtraCodecs (fromRecordN, fromRequired, toInt, toRecordN, toRequired, toString)

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

associativityJSON :: Associativity -> JSON
associativityJSON = JSON.fromString <<< case _ of
  Infixl -> "infixl"
  Infixr -> "infixr"
  Infix -> "infix"

jsonAssociativity :: JSON -> Either String Associativity
jsonAssociativity = toString >=> case _ of
  "infixl" -> pure Infixl
  "infixr" -> pure Infixr
  "infix" -> pure Infix
  str -> Left $ "Expected 'infixl', 'infixr', or 'infix' but got '" <> str <> "'"

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

fixityJSON :: Fixity -> JSON
fixityJSON = fromRecordN Fixity
  { associativity: fromRequired associativityJSON
  , precedence: fromRequired JSON.fromInt
  }

jsonFixity :: JSON -> Either String Fixity
jsonFixity = toRecordN Fixity
  { associativity: toRequired jsonAssociativity
  , precedence: toRequired toInt
  }
