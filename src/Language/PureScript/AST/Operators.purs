-- | Operators fixity and associativity
module Language.PureScript.AST.Operators where

import Prelude

import Codec.Json.Unidirectional.Value (DecodeError(..), fromRecordN, fromRequired, toInt, toRecordN, toRequired, toString)
import Codec.Json.Unidirectional.Value as Json
import Data.Argonaut.Core (Json)
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafeCrashWith)

-- | A precedence level for an infix operator
type Precedence = Int

-- | Associativity for infix operators
data Associativity
  = Infixl
  | Infixr
  | Infix

derive instance Eq Associativity
derive instance Ord Associativity
derive instance Generic Associativity _
instance Show Associativity where
  show x = genericShow x

showAssoc :: Associativity -> String
showAssoc Infixl = "infixl"
showAssoc Infixr = "infixr"
showAssoc Infix = "infix"

readAssoc :: String -> Associativity
readAssoc "infixl" = Infixl
readAssoc "infixr" = Infixr
readAssoc "infix" = Infix
readAssoc _ = unsafeCrashWith "readAssoc: no parse"

readAssoc' :: String -> Maybe Associativity
readAssoc' "infixl" = Just Infixl
readAssoc' "infixr" = Just Infixr
readAssoc' "infix" = Just Infix
readAssoc' _ = Nothing

fromAssociativity :: Associativity -> Json
fromAssociativity = Json.fromString <<< showAssoc

toAssociativity :: Json -> Either Json.DecodeError Associativity
toAssociativity = toString >=> \str -> do
  let errorMsg = DecodeError $ "Expected 'infixl', 'infixr', or 'infix' but got '" <> str <> "'"
  note errorMsg $ readAssoc' str

-- | Fixity data for infix operators
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
