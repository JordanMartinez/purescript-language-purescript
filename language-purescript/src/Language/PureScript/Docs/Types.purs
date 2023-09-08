module Language.PureScript.Docs.Types where

import Prelude
import Prim hiding (Type, Constraint)

import Codec.Json.Unidirectional.Value (ToProp(..))
import Codec.Json.Unidirectional.Value as J
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Bitraversable (bitraverse)
import Data.DateTime (DateTime)
import Data.Either (Either(..), note)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format, unformat)
import Data.Function.Uncurried (mkFn2)
import Data.Functor (voidRight)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Version (Version, showVersion, version)
import Data.Version as Version
import Foreign.Object as Object
import Language.PureScript.AST.Declarations (KindSignatureFor, jsonKindSignatureFor, kindSignatureForJSON)
import Language.PureScript.AST.Operators (Fixity, fixityJSON, jsonFixity)
import Language.PureScript.AST.SourcePos (SourceSpan, jsonSourceSpan, sourceSpanJSON)
import Language.PureScript.Docs.RenderedCode.Types (FixityAlias, fixityAliasJSON, jsonFixityAlias)
import Language.PureScript.Docs.RenderedCode.Types as DRCTypes
import Language.PureScript.Environment (DataDeclType, dataDeclTypeJSON, jsonDataDeclType, kindType)
import Language.PureScript.Names (ModuleName(..), jsonModuleName, jsonProperName, jsonQualified, moduleNameJSON)
import Language.PureScript.Publish.Registry.Compat (PursJsonError)
import Language.PureScript.Role (Role, jsonRole, roleJSON)
import Language.PureScript.Types (Constraint(..), constraintJSON, jsonTypeUnit, typeJSON)
import Language.PureScript.Types as P
import Safe.Coerce (coerce)
import Web.Bower.PackageMeta (BowerError, PackageMeta, PackageName, jsonPackageMeta, jsonPackageName, packageMetaJSON, packageNameJSON, parsePackageName)

type Type' = P.Type Unit
type Constraint' = P.Constraint Unit

type UploadedPackage = Package NotYetKnown
type VerifiedPackage = Package GithubUser

newtype Package a = Package
  { packageMeta :: PackageMeta
  , version :: Version
  , versionTag :: String
  , tagTime :: Maybe DateTime
  , modules :: Array DocModule
  , moduleMap :: Map ModuleName PackageName
  , resolvedDependencies :: Array (Tuple PackageName Version)
  , github :: Tuple GithubUser GithubRepo
  , uploader :: a
  , compilerVersion :: Version
  }

derive instance Eq a => Eq (Package a)
derive instance Ord a => Ord (Package a)
derive instance Newtype (Package a) _
derive instance Generic (Package a) _
instance Show a => Show (Package a) where
  show x = genericShow x

packageJSON :: forall a. (a -> Json) -> Package a -> Json
packageJSON uploaderJSON = J.fromRecordN Package
  { packageMeta: J.fromRequired packageMetaJSON
  , version: J.fromRequired versionJSON
  , versionTag: J.fromRequired J.fromString
  , modules: J.fromRequired $ J.fromArray docModuleJSON
  , moduleMap: J.fromRequired $ (J.fromPropArray <<< map (bimap unwrap packageNameJSON) <<< Map.toUnfoldable)
  , resolvedDependencies: J.fromRequired $ (J.fromPropArray <<< map (bimap unwrap versionJSON))
  , github: J.fromRequired $ J.fromTuple githubUserJSON githubRepoJSON
  , uploader: J.fromRequired uploaderJSON
  , compilerVersion: J.fromRequired versionJSON
  , tagTime: J.fromOption iso8601JSON
  }

jsonUploadedPackage :: Version -> Json -> Either J.DecodeError UploadedPackage
jsonUploadedPackage minVersion = jsonPackage minVersion jsonNotYetKnown

jsonPackage :: forall a. Version -> (Json -> Either J.DecodeError a) -> Json -> Either J.DecodeError (Package a)
jsonPackage minimumVersion jsonUploader j = do
  jo <- J.toJObject j
  compilerVersion <- ((J.underKey "compilerVersion" jsonVersion jo) <|> (pure $ version 0 7 0 Nil Nil))
  when (compilerVersion < minimumVersion)
    (Left $ J.DecodeError $ "Invalid compiler version: " <> showVersion compilerVersion :: Either J.DecodeError Unit)
  J.toRecordN Package
    { packageMeta: J.toRequired jsonPackageMeta
    , version: J.toRequired jsonVersion
    , versionTag: J.toRequired J.toString
    , tagTime: J.toOption jsonIso8601
    , modules: J.toRequired $ J.toArray jsonDocModule
    , moduleMap: ToProp $ mkFn2 \lookup key -> do
        let
          toModuleMap = lmap (J.AtKey key) do
            case lookup key of
              Nothing ->
                Left $ J.DecodeError "Missing key"
              Just x ->
                (Object.toUnfoldable <$> J.toJObject x)
                  >>= flip Array.foldM Map.empty
                    ( \acc (Tuple k v) -> do
                        v' <- jsonPackageName v
                        pure $ Map.insert (ModuleName k) v' acc
                    )

          -- This is here to preserve backwards compatibility with compilers which used
          -- to generate a 'bookmarks' field in the Json (i.e. up to 0.10.5). We should
          -- remove this after the next breaking change to the J.
          toBookmarks _ = lmap (J.AtKey "bookmarks") do
            case lookup "bookmarks" of
              Nothing ->
                Left $ J.DecodeError "Missing key"
              Just x -> do
                arr <- J.toArray (jsonInPackage (J.toJArray >=> J.underIndex 0 jsonModuleName)) x
                pure $ Map.fromFoldable $ flip Array.mapMaybe arr case _ of
                  Local _ -> Nothing
                  FromDep pkgName mn -> Just $ Tuple mn pkgName

        J.altAccumulateLazy toModuleMap toBookmarks

    , resolvedDependencies: J.toRequired $ J.toJObject >>> map Object.toUnfoldable >=> traverse (bitraverse (map (note $ J.DecodeError "Invalid package name") parsePackageName) jsonVersion)
    , github: J.toRequired $ J.toTuple jsonGithubUser jsonGithubRepo
    , uploader: J.toRequired jsonUploader
    , compilerVersion: J.toStatic compilerVersion
    }
    j

yyyy_mm_dd :: Array FormatterCommand
yyyy_mm_dd =
  [ YearFull
  , Placeholder "-"
  , MonthTwoDigits
  , Placeholder "-"
  , DayOfMonthTwoDigits
  ]

hh_mm :: Array FormatterCommand
hh_mm =
  [ Hours24
  , Placeholder ":"
  , MinutesTwoDigits
  ]

_ss :: Array FormatterCommand
_ss = [ Placeholder ":", SecondsTwoDigits ]

_sss :: Array FormatterCommand
_sss = [ Placeholder ":", Milliseconds ]

iso8601JSON :: DateTime -> Json
iso8601JSON = J.fromString <<< format encoder
  where
  encoder = List.fromFoldable $
    yyyy_mm_dd <> [ Placeholder "T" ] <> hh_mm <> _ss <> _sss <> [ Placeholder "Z" ]

jsonIso8601 :: Json -> Either J.DecodeError DateTime
jsonIso8601 = J.toString >=> unformat primaryDecoder >>> lmap J.DecodeError
  where
  yyyy_mm_ddThh_mm = (Array.snoc yyyy_mm_dd (Placeholder "T")) <> hh_mm

  -- mainDecoders :: Array Formatter
  -- mainDecoders = do
  --   sign <- [ [ Placeholder "+" ], [ Placeholder "-" ], [] ]
  --   _ss' <- [ _ss, [] ]
  --   _sss' <- [ _sss, [] ]
  --   pure $ List.fromFoldable $ sign <> yyyy_mm_ddThh_mm <> _ss' <> _sss' <> [ Placeholder "Z" ]

  primaryDecoder :: Formatter
  primaryDecoder = List.fromFoldable $ yyyy_mm_ddThh_mm <> _ss <> [ Placeholder "+0000" ]

data NotYetKnown = NotYetKnown

derive instance Eq NotYetKnown
derive instance Ord NotYetKnown
derive instance Generic NotYetKnown _
instance Show NotYetKnown where
  show x = genericShow x

notYetKnownJSON :: NotYetKnown -> Json
notYetKnownJSON = const J.fromJNull

jsonNotYetKnown :: Json -> Either J.DecodeError NotYetKnown
jsonNotYetKnown = voidRight NotYetKnown <<< J.toJNull

data ManifestError
  = BowerManifest BowerError
  | PursManifest PursJsonError

derive instance Eq ManifestError
derive instance Ord ManifestError
derive instance Generic ManifestError _
instance Show ManifestError where
  show x = genericShow x

newtype DocModule = DocModule
  { name :: ModuleName
  , comments :: Maybe String
  , declarations :: Array Declaration
  , reExports :: Array (Tuple (InPackage ModuleName) (Array Declaration))
  }

derive instance Eq DocModule
derive instance Ord DocModule
derive instance Newtype DocModule _
derive instance Generic DocModule _
instance Show DocModule where
  show x = genericShow x

docModuleJSON :: DocModule -> Json
docModuleJSON = J.fromRecordN DocModule
  { name: J.fromRequired $ unwrap >>> J.fromString
  , comments: J.fromOption J.fromString
  , declarations: J.fromRequired $ J.fromArray declarationJSON
  , reExports: J.fromRequired $ J.fromArray fromObj
  }
  where
  fromObj :: Tuple (InPackage ModuleName) (Array Declaration) -> Json
  fromObj (Tuple mn decls) = J.fromPropArray
    [ Tuple "moduleName" $ inPackageJSON moduleNameJSON mn
    , Tuple "declarations" $ J.fromArray declarationJSON decls
    ]

jsonDocModule :: Json -> Either J.DecodeError DocModule
jsonDocModule = J.toRecordN DocModule
  { name: J.toRequired $ coerce <<< J.toString
  , comments: J.toRequired $ J.toNullNothingOrJust J.toString
  , declarations: J.toRequired $ J.toArray jsonDeclaration
  , reExports: J.toRequired $ J.toArray jsonAsReExport
  }
  where
  jsonAsReExport :: Json -> Either J.DecodeError (Tuple (InPackage ModuleName) (Array Declaration))
  jsonAsReExport =
    J.toRecord
      { moduleName: J.toRequired jsonAsRexportModuleName
      , declarations: J.toRequired $ J.toArray jsonDeclaration
      }
      >>> map \{ moduleName, declarations } -> Tuple moduleName declarations

  -- This is to preserve backwards compatibility with 0.10.3 and earlier versions
  -- of the compiler, where the modReExports field had the type
  -- [(P.ModuleName, [Declaration])]. This should eventually be removed,
  -- possibly at the same time as the next breaking change to this Json format.
  jsonAsRexportModuleName :: Json -> Either J.DecodeError (InPackage ModuleName)
  jsonAsRexportModuleName = J.altAccumulate
    (jsonInPackage jsonModuleName)
    (map Local <$> jsonModuleName)

newtype Declaration = Declaration
  { title :: String
  , comments :: Maybe String
  , sourceSpan :: Maybe SourceSpan
  , children :: Array ChildDeclaration
  , info :: DeclarationInfo
  , kind :: Maybe KindInfo
  }

derive instance Eq Declaration
derive instance Ord Declaration
derive instance Newtype Declaration _
derive instance Generic Declaration _
instance Show Declaration where
  show x = genericShow x

declarationJSON :: Declaration -> Json
declarationJSON = J.fromRecordN Declaration
  { title: J.fromRequired J.fromString
  , comments: J.fromRequired $ J.fromNullNothingOrJust J.fromString
  , sourceSpan: J.fromRequired $ J.fromNullNothingOrJust sourceSpanJSON
  , children: J.fromRequired $ J.fromArray childDeclarationJSON
  , info: J.fromRequired declarationInfoJSON
  , kind: J.fromOption kindInfoJSON
  }

jsonDeclaration :: Json -> Either J.DecodeError Declaration
jsonDeclaration = J.toRecordN Declaration
  { title: J.toRequired J.toString
  , comments: J.toRequired $ J.toNullNothingOrJust J.toString
  , sourceSpan: J.toRequired $ J.toNullNothingOrJust jsonSourceSpan
  , children: J.toRequired $ J.toArray jsonChildDeclaration
  , info: J.toRequired jsonDeclarationInfo
  , kind: J.toOptionDefault Nothing $ J.toNullNothingOrJust jsonKindInfo
  }

data DeclarationInfo
  = ValueDeclaration Type'
  | DataDeclaration DataDeclType (Array (Tuple String (Maybe Type'))) (Array Role)
  | ExternDataDeclaration Type' (Array Role)
  | TypeSynonymDeclaration (Array (Tuple String (Maybe Type'))) Type'
  | TypeClassDeclaration (Array (Tuple String (Maybe Type'))) (Array Constraint') (Array (Tuple (Array String) (Array String)))
  | AliasDeclaration Fixity FixityAlias

derive instance Eq DeclarationInfo
derive instance Ord DeclarationInfo
derive instance Generic DeclarationInfo _
instance Show DeclarationInfo where
  show x = genericShow x

declarationInfoJSON :: DeclarationInfo -> Json
declarationInfoJSON = case _ of
  ValueDeclaration ty -> J.fromPropArray
    [ Tuple "declType" $ J.fromString "value"
    , Tuple "type " $ typeJSON (const J.fromJNull) ty
    ]
  DataDeclaration ty args roles -> J.fromPropArray
    [ Tuple "declType" $ J.fromString "data"
    , Tuple "dataDeclType" $ dataDeclTypeJSON ty
    , Tuple "typeArguments" $ J.fromArray (J.fromTuple J.fromString (J.fromNullNothingOrJust (typeJSON (const J.fromJNull)))) args
    , Tuple "roles" $ J.fromArray roleJSON roles
    ]
  ExternDataDeclaration kind roles -> J.fromPropArray
    [ Tuple "declType" $ J.fromString "externData"
    , Tuple "kind" $ typeJSON (const J.fromJNull) kind
    , Tuple "roles" $ J.fromArray roleJSON roles
    ]
  TypeSynonymDeclaration args ty -> J.fromPropArray
    [ Tuple "declType" $ J.fromString "typeSynonym"
    , Tuple "arguments" $ J.fromArray (J.fromTuple J.fromString (J.fromNullNothingOrJust (typeJSON (const J.fromJNull)))) args
    , Tuple "type" $ typeJSON (const J.fromJNull) ty
    ]
  TypeClassDeclaration args super fundeps -> J.fromPropArray
    [ Tuple "declType" $ J.fromString "typeClass"
    , Tuple "arguments" $ J.fromArray (J.fromTuple J.fromString (J.fromNullNothingOrJust (typeJSON (const J.fromJNull)))) args
    , Tuple "superclasses" $ J.fromArray (constraintJSON (const J.fromJNull)) super
    , Tuple "fundeps" $ J.fromArray (J.fromTuple (J.fromArray J.fromString) (J.fromArray J.fromString)) fundeps
    ]
  AliasDeclaration fixity alias -> J.fromPropArray
    [ Tuple "declType" $ J.fromString "alias"
    , Tuple "fixity" $ fixityJSON fixity
    , Tuple "alias" $ fixityAliasJSON alias
    ]

jsonDeclarationInfo :: Json -> Either J.DecodeError DeclarationInfo
jsonDeclarationInfo j = do
  jo <- J.toJObject j
  declType <- J.underKey "declType" J.toString jo
  case declType of
    "value" ->
      ValueDeclaration
        <$> (J.underKey "type" jsonTypeUnit jo)
    "data" ->
      DataDeclaration
        <$> (J.underKey "dataDeclType" jsonDataDeclType jo)
        <*> (J.underKey "typeArguments" jsonTypeArguments jo)
        <*> ((J.underKey "roles" (J.toArray jsonRole) jo) <|> pure [])
    "externData" ->
      ExternDataDeclaration
        <$> (J.underKey "kind" jsonTypeUnit jo)
        <*> ((J.underKey "roles" (J.toArray jsonRole) jo) <|> pure [])
    "typeSynonym" ->
      TypeSynonymDeclaration
        <$> (J.underKey "arguments" jsonTypeArguments jo)
        <*> (J.underKey "type" jsonTypeUnit jo)
    "typeClass" ->
      TypeClassDeclaration
        <$> (J.underKey "arguments" jsonTypeArguments jo)
        <*> (J.underKey "superclasses" (J.toArray jsonAsConstrantUnit) jo)
        <*> ((J.underKey "fundeps" jsonFunDeps jo) <|> pure [])
    "alias" ->
      AliasDeclaration
        <$> (J.underKey "fixity" jsonFixity jo)
        <*> (J.underKey "alias" jsonFixityAlias jo)
    -- Backwards compat: kinds are extern data
    "kind" ->
      pure $ ExternDataDeclaration (void kindType) []
    str ->
      Left $ J.DecodeError $ "Expected 'instance', 'dataConstructor', or 'typeClassMember' but got '" <> str <> "'."

jsonTypeArguments :: Json -> Either J.DecodeError (Array (Tuple String (Maybe Type')))
jsonTypeArguments = J.toArray (J.toTuple J.toString (J.toNullNothingOrJust jsonTypeUnit))

jsonAsConstrantUnit :: Json -> Either J.DecodeError Constraint'
jsonAsConstrantUnit = J.toRecordN Constraint
  { ann: J.toStatic unit
  , class: J.toRequiredRename "constraintClass" $ jsonQualified jsonProperName
  , kindArgs: J.toOptionDefaultRename "constraintKindArgs" [] $ J.toArray jsonTypeUnit
  , args: J.toRequiredRename "constraintArgs" $ J.toArray jsonTypeUnit
  , data: J.toStatic Nothing
  }

newtype KindInfo = KindInfo
  { keyword :: KindSignatureFor
  , kind :: Type'
  }

derive instance Eq KindInfo
derive instance Ord KindInfo
derive instance Newtype KindInfo _
derive instance Generic KindInfo _
instance Show KindInfo where
  show x = genericShow x

kindInfoJSON :: KindInfo -> Json
kindInfoJSON = J.fromRecordN KindInfo
  { keyword: J.fromRequired kindSignatureForJSON
  , kind: J.fromRequired $ typeJSON (const J.fromJNull)
  }

jsonKindInfo :: Json -> Either J.DecodeError KindInfo
jsonKindInfo = J.toRecordN KindInfo
  { keyword: J.toRequired jsonKindSignatureFor
  , kind: J.toRequired jsonTypeUnit
  }

newtype ChildDeclaration = ChildDeclaration
  { title :: String
  , comments :: Maybe String
  , sourceSpan :: Maybe SourceSpan
  , info :: ChildDeclarationInfo
  }

derive instance Eq ChildDeclaration
derive instance Ord ChildDeclaration
derive instance Newtype ChildDeclaration _
derive instance Generic ChildDeclaration _
instance Show ChildDeclaration where
  show x = genericShow x

childDeclarationJSON :: ChildDeclaration -> Json
childDeclarationJSON = J.fromRecordN ChildDeclaration
  { title: J.fromRequired J.fromString
  , comments: J.fromRequired $ J.fromNullNothingOrJust J.fromString
  , sourceSpan: J.fromRequired $ J.fromNullNothingOrJust sourceSpanJSON
  , info: J.fromRequired childDeclarationInfoJSON
  }

jsonChildDeclaration :: Json -> Either J.DecodeError ChildDeclaration
jsonChildDeclaration = J.toRecordN ChildDeclaration
  { title: J.toRequired J.toString
  , comments: J.toRequired $ J.toNullNothingOrJust J.toString
  , sourceSpan: J.toRequired $ J.toNullNothingOrJust jsonSourceSpan
  , info: J.toRequired jsonChildDeclarationInfo
  }

data ChildDeclarationInfo
  = ChildInstance (Array Constraint') Type'
  | ChildDataConstructor (Array Type')
  | ChildTypeClassMember Type'

derive instance Eq ChildDeclarationInfo
derive instance Ord ChildDeclarationInfo
derive instance Generic ChildDeclarationInfo _
instance Show ChildDeclarationInfo where
  show x = genericShow x

childDeclarationInfoJSON :: ChildDeclarationInfo -> Json
childDeclarationInfoJSON = case _ of
  ChildInstance deps ty -> J.fromPropArray
    [ Tuple "declType" $ J.fromString "instance"
    , Tuple "dependencies" $ J.fromArray (constraintJSON (const J.fromJNull)) deps
    , Tuple "type" $ typeJSON (const J.fromJNull) ty
    ]
  ChildDataConstructor args -> J.fromPropArray
    [ Tuple "declType" $ J.fromString "typeClassMember"
    , Tuple "arguments" $ J.fromArray (typeJSON (const J.fromJNull)) args
    ]
  ChildTypeClassMember ty -> J.fromPropArray
    [ Tuple "declType" $ J.fromString "typeClassMember"
    , Tuple "type" $ typeJSON (const J.fromJNull) ty
    ]

jsonChildDeclarationInfo :: Json -> Either J.DecodeError ChildDeclarationInfo
jsonChildDeclarationInfo j = do
  jo <- J.toJObject j
  declType <- J.underKey "declType" J.toString jo
  case declType of
    "instance" ->
      ChildInstance
        <$> (J.underKey "dependencies" (J.toArray jsonAsConstrantUnit) jo)
        <*> (J.underKey "type" jsonTypeUnit jo)
    "dataConstructor" ->
      ChildDataConstructor
        <$> (J.underKey "arguments" (J.toArray jsonTypeUnit) jo)
    "typeClassMember" ->
      ChildTypeClassMember
        <$> (J.underKey "type" jsonTypeUnit jo)
    str ->
      Left $ J.DecodeError $ "Expected 'instance', 'dataConstructor', or 'typeClassMember' but got '" <> str <> "'."

newtype GithubUser = GithubUser String

derive instance Eq GithubUser
derive instance Ord GithubUser
derive instance Newtype GithubUser _
derive instance Generic GithubUser _
instance Show GithubUser where
  show x = genericShow x

githubUserJSON :: GithubUser -> Json
githubUserJSON = unwrap >>> J.fromString

jsonGithubUser :: Json -> Either J.DecodeError GithubUser
jsonGithubUser = coerce <<< J.toString

newtype GithubRepo = GithubRepo String

derive instance Eq GithubRepo
derive instance Ord GithubRepo
derive instance Newtype GithubRepo _
derive instance Generic GithubRepo _
instance Show GithubRepo where
  show x = genericShow x

githubRepoJSON :: GithubRepo -> Json
githubRepoJSON = unwrap >>> J.fromString

jsonGithubRepo :: Json -> Either J.DecodeError GithubRepo
jsonGithubRepo = coerce <<< J.toString

data PackageError
  = CompilerTooOld Version Version
  | ErrorInPackageMeta ManifestError
  | InvalidVersion
  | InvalidDeclarationType String
  | InvalidChildDeclarationType String
  | InvalidFixity
  | InvalidKind String
  | InvalidDataDeclType String
  | InvalidKindSignatureFor String
  | InvalidTime
  | InvalidRole String

derive instance Eq PackageError
derive instance Ord PackageError
derive instance Generic PackageError _
instance Show PackageError where
  show x = genericShow x

data InPackage a
  = Local a
  | FromDep PackageName a

derive instance Eq a => Eq (InPackage a)
derive instance Ord a => Ord (InPackage a)
derive instance Generic (InPackage a) _
instance Show a => Show (InPackage a) where
  show x = genericShow x

derive instance Functor InPackage

inPackageJSON :: forall a. (a -> Json) -> InPackage a -> Json
inPackageJSON innerJSON = case _ of
  Local a -> J.fromPropArray
    [ Tuple "item" $ innerJSON a ]
  FromDep pkgName a -> J.fromPropArray
    [ Tuple "package" $ packageNameJSON pkgName
    , Tuple "item" $ innerJSON a
    ]

jsonInPackage :: forall a. (Json -> Either J.DecodeError a) -> Json -> Either J.DecodeError (InPackage a)
jsonInPackage jsonInner =
  J.toRecord
    { package: J.toOptionDefault Nothing $ J.toNullNothingOrJust jsonPackageName
    , item: J.toRequired jsonInner
    }
    >>> map \{ package, item } -> maybe (Local item) (flip FromDep item) package

----------------------------------------------------
-- Types for links between declarations

newtype LinksContext = LinksContext
  { github :: Tuple GithubUser GithubRepo
  , moduleMap :: Map ModuleName PackageName
  , resolvedDependencies :: Array (Tuple PackageName Version)
  , packageName :: PackageName
  , version :: Version
  , versionTag :: String
  }

derive instance Eq LinksContext
derive instance Ord LinksContext
derive instance Newtype LinksContext _
derive instance Generic LinksContext _
instance Show LinksContext where
  show x = genericShow x

newtype DocLink = DocLink
  { location :: LinkLocation
  , title :: String
  , namespace :: DRCTypes.Namespace
  }

derive instance Eq DocLink
derive instance Ord DocLink
derive instance Newtype DocLink _
derive instance Generic DocLink _
instance Show DocLink where
  show x = genericShow x

data LinkLocation
  = LocalModule ModuleName
  | DepsModule PackageName Version ModuleName
  | BuiltinModule ModuleName

derive instance Eq LinkLocation
derive instance Ord LinkLocation
derive instance Generic LinkLocation _
instance Show LinkLocation where
  show x = genericShow x

jsonFunDeps :: Json -> Either J.DecodeError (Array (Tuple (Array String) (Array String)))
jsonFunDeps = J.toArray (J.toTuple (J.toArray J.toString) (J.toArray J.toString))

versionJSON :: Version -> Json
versionJSON = J.fromString <<< showVersion

jsonVersion :: Json -> Either J.DecodeError Version
jsonVersion = J.toString >=> Version.parseVersion >>> lmap (show >>> J.DecodeError)
