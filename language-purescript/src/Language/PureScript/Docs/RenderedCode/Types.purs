module Language.PureScript.Docs.RenderedCode.Types where

import Prelude

import Codec.Json.Unidirectional.Value as Json
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Language.PureScript.Names (ConstructorName, Ident, ModuleName, ProperName, Qualified, TypeName, identJSON, jsonIdent, jsonModuleName, jsonProperName, jsonQualified, moduleNameJSON, properNameJSON, qualifiedJSON)
import Safe.Coerce (coerce)

data ContainingModule
  = ThisModule
  | OtherModule ModuleName

derive instance Eq ContainingModule
derive instance Ord ContainingModule
derive instance Generic ContainingModule _
instance Show ContainingModule where
  show x = genericShow x

containingModuleJSON :: ContainingModule -> Json
containingModuleJSON = Json.fromJArray <<< case _ of
  ThisModule -> [ Json.fromString "ThisModule" ]
  OtherModule mn -> [ Json.fromString "OtherModule", moduleNameJSON mn ]

jsonContainingModule :: Json -> Either Json.DecodeError ContainingModule
jsonContainingModule j' = current j' <|> backwardsCompat j'
  where
  current = Json.toJArray >=> \ja -> do
    tag <- Json.underIndex 0 Json.toString ja
    case tag of
      "ThisModule" -> pure ThisModule
      "OtherModule" -> OtherModule <$> Json.underIndex 1 jsonModuleName ja
      str -> Left $ Json.DecodeError $ "Expected 'ThisModule' or 'OtherModule' but got '" <> str <> "'."

  -- For Json produced by compilers up to 0.10.5.
  backwardsCompat =
    Json.toNullNothingOrJust jsonModuleName
      >>> map (maybe ThisModule OtherModule)

data Link
  = NoLink
  | Link ContainingModule

derive instance Eq Link
derive instance Ord Link
derive instance Generic Link _
instance Show Link where
  show x = genericShow x

linkJSON :: Link -> Json
linkJSON = Json.fromJArray <<< case _ of
  NoLink -> [ Json.fromString "NoLink" ]
  Link cm -> [ Json.fromString "Link", containingModuleJSON cm ]

jsonLink :: Json -> Either Json.DecodeError Link
jsonLink = Json.toJArray >=> \ja -> do
  tag <- Json.underIndex 0 Json.toString ja
  case tag of
    "NoLink" -> pure NoLink
    "Link" -> Json.underIndex 1 (jsonContainingModule >>> map Link) ja
    str -> Left $ Json.DecodeError $ "Expected 'NoLink' or 'Link' but got '" <> str <> "'."

data Namespace
  = ValueLevel
  | TypeLevel

derive instance Eq Namespace
derive instance Ord Namespace
derive instance Generic Namespace _
instance Show Namespace where
  show x = genericShow x

namespaceJSON :: Namespace -> Json
namespaceJSON = Json.fromString <<< case _ of
  ValueLevel -> "ValueLevel"
  TypeLevel -> "TypeLevel"

jsonNamespace :: Json -> Either Json.DecodeError Namespace
jsonNamespace = Json.toString >=> case _ of
  "ValueLevel" -> pure ValueLevel
  "TypeLevel" -> pure TypeLevel
  str -> Left $ Json.DecodeError $ "Expected 'ValueLevel' or 'TypeLevel' but got '" <> str <> "'."

-- |
-- A single element in a rendered code fragment. The intention is to support
-- multiple output formats. For example, plain text, or highlighted HTML.
--
data RenderedCodeElement
  = Syntax String
  | Keyword String
  | Space
  -- | Any symbol which you might or might not want to link to, in any
  -- namespace (value, type, or kind). Note that this is not related to the
  -- kind called Symbol for type-level strings.
  | Symbol Namespace String Link
  | Role String

derive instance Eq RenderedCodeElement
derive instance Ord RenderedCodeElement
derive instance Generic RenderedCodeElement _
instance Show RenderedCodeElement where
  show x = genericShow x

renderedCodeElementJSON :: RenderedCodeElement -> Json
renderedCodeElementJSON = Json.fromJArray <<< case _ of
  Syntax str -> [ Json.fromString "syntax", Json.fromString str ]
  Keyword str -> [ Json.fromString "keyword", Json.fromString str ]
  Space -> [ Json.fromString "space" ]
  Symbol ns str link -> [ Json.fromString "symbol", namespaceJSON ns, Json.fromString str, linkJSON link ]
  Role role -> [ Json.fromString "role", Json.fromString role ]

jsonRenderedCodeElement :: Json -> Either Json.DecodeError RenderedCodeElement
jsonRenderedCodeElement = Json.toJArray >=> \ja -> do
  ty <- Json.underIndex 0 Json.toString ja
  case ty of
    "syntax" ->
      Syntax <$> (Json.underIndex 1 Json.toString ja)
    "keyword" ->
      Keyword <$> (Json.underIndex 1 Json.toString ja)
    "space" ->
      pure Space
    "symbol" ->
      Symbol
        <$> (Json.underIndex 1 jsonNamespace ja)
        <*> (Json.underIndex 2 Json.toString ja)
        <*> (Json.underIndex 3 jsonLink ja)
    "role" ->
      Role <$> (Json.underIndex 1 Json.toString ja)
    str -> Left $ Json.DecodeError $ "Expected 'syntax', 'keyword', 'space', 'symbol' or 'role' but got " <> str <> "'."

newtype RenderedCode = RenderedCode (Array RenderedCodeElement)

derive instance Eq RenderedCode
derive instance Ord RenderedCode
derive instance Newtype RenderedCode _
derive instance Generic RenderedCode _
instance Show RenderedCode where
  show x = genericShow x

derive newtype instance Semigroup RenderedCode
derive newtype instance Monoid RenderedCode

renderedCodeJSON :: RenderedCode -> Json
renderedCodeJSON = unwrap >>> Json.fromArray renderedCodeElementJSON

jsonRenderedCode :: Json -> Either Json.DecodeError RenderedCode
jsonRenderedCode = coerce <<< Json.toArray jsonRenderedCodeElement

type FixityAlias = Qualified (Either (ProperName TypeName) (Either Ident (ProperName ConstructorName)))

fixityAliasJSON :: FixityAlias -> Json
fixityAliasJSON = qualifiedJSON $ eitherJSON properNameJSON $ eitherJSON identJSON properNameJSON
  where
  eitherJSON :: forall a b. (a -> Json) -> (b -> Json) -> Either a b -> Json
  eitherJSON l' r' = case _ of
    Left l -> Json.fromObjSingleton "Left" $ l' l
    Right r -> Json.fromObjSingleton "Right" $ r' r

jsonFixityAlias :: Json -> Either Json.DecodeError FixityAlias
jsonFixityAlias = jsonQualified $ jsonEither jsonProperName $ jsonEither jsonIdent jsonProperName
  where
  jsonEither :: forall a b. (Json -> Either Json.DecodeError a) -> (Json -> Either Json.DecodeError b) -> Json -> Either Json.DecodeError (Either a b)
  jsonEither l' r' j = left <|> right
    where
    left = Json.toObjSingleton "Left" (l' >>> map Left) j
    right = Json.toObjSingleton "Right" (r' >>> map Right) j
