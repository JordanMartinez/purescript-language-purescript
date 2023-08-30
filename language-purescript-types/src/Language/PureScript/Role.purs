module Language.PureScript.Role where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import JSON (JSON)
import JSON as JSON
import JSON.ExtraCodecs (toString)

-- |
-- The role of a type constructor's parameter.
data Role
  = Nominal
  | Representational
  | Phantom

derive instance Eq Role
derive instance Ord Role
derive instance Generic Role _
instance Show Role where
  show x = genericShow x

roleJSON :: Role -> JSON
roleJSON = JSON.fromString <<< case _ of
  Representational -> "Representational"
  Nominal -> "Nominal"
  Phantom -> "Phantom"

jsonRole :: JSON -> Either String Role
jsonRole = toString >=> case _ of
  "Representational" -> pure Representational
  "Nominal" -> pure Nominal
  "Phantom" -> pure Phantom
  str -> Left $ "Expected 'Representational', 'Nominal', or 'Phantom', but got '" <> str <> "'."
