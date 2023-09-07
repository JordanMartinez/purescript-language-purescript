module Language.PureScript.Publish.Registry.Compat where

import Prelude

import Codec.Json.Unidirectional.Value as Json
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Foreign.Object as Object

-- | A partial representation of the purs.json manifest format, including only
-- | the fields required for publishing.
-- |
-- | https://github.com/purescript/registry/blob/master/v1/Manifest.dhall
--
-- This type is intended for compatibility with the Bower publishing pipeline,
-- and does not accurately reflect all possible purs.json manifests. However,
-- supporting purs.json manifests properly introduces breaking changes to the
-- compiler and to Pursuit.
newtype PursJson = PursJson
  { name :: String
  , license :: String
  , location :: String
  , description :: Maybe String
  , dependencies :: Map String String
  }

derive instance Eq PursJson
derive instance Newtype PursJson _
derive instance Generic PursJson _
instance Show PursJson where
  show x = genericShow x

jsonPursJson :: Json -> Either Json.DecodeError PursJson
jsonPursJson = Json.toRecordN PursJson
  { name: Json.toRequired Json.toString
  , description: Json.toOption Json.toString
  , license: Json.toRequired Json.toString
  , dependencies: Json.toRequired $
      Json.toJObject
        >>> map Object.toUnfoldable
        >=> (traverse (traverse Json.toString) :: Array _ -> _)
          >>> map Map.fromFoldable
  , location: Json.toRequired jsonOwnerRepoOrGitUrl
  }
  where
  jsonOwnerRepoOrGitUrl j = asOwnerRepo j <|> asGitUrl j
    where
    asOwnerRepo =
      Json.toRecord
        { githubOwner: Json.toRequired Json.toString
        , githubRepo: Json.toRequired Json.toString
        }
        >>> map \{ githubOwner, githubRepo } -> "https://github.com/" <> githubOwner <> "/" <> githubRepo <> ".git"

    asGitUrl = Json.toJObject >=> Json.underKey "gitUrl" Json.toString

data PursJsonError = MalformedLocationField

derive instance Eq PursJsonError
derive instance Ord PursJsonError
derive instance Generic PursJsonError _
instance Show PursJsonError where
  show x = genericShow x
