module Language.PureScript.Publish.Registry.Compat where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import JSON (JSON)
import JSON.ExtraCodecs (toJObject, toOption, toRecord, toRecordN, toRequired, toString, underRequiredKey)
import JSON.Object as JO

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

jsonPursJson :: JSON -> Either String PursJson
jsonPursJson = toRecordN PursJson
  { name: toRequired toString
  , description: toOption toString
  , license: toRequired toString
  , dependencies: toRequired $
      toJObject
        >>> map JO.toUnfoldable
        >=> (traverse (traverse toString) :: Array _ -> _)
          >>> map Map.fromFoldable
  , location: toRequired jsonOwnerRepoOrGitUrl
  }
  where
  jsonOwnerRepoOrGitUrl j = asOwnerRepo j <|> asGitUrl j
    where
    asOwnerRepo =
      toRecord
        { githubOwner: toRequired toString
        , githubRepo: toRequired toString
        }
        >>> map \{ githubOwner, githubRepo } -> "https://github.com/" <> githubOwner <> "/" <> githubRepo <> ".git"

    asGitUrl = toJObject >=> \jo -> underRequiredKey "gitUrl" jo toString

data PursJsonError = MalformedLocationField

derive instance Eq PursJsonError
derive instance Ord PursJsonError
derive instance Generic PursJsonError _
instance Show PursJsonError where
  show x = genericShow x
