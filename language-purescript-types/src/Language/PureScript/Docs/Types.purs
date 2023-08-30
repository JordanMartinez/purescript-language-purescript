module Language.PureScript.Docs.Types where

import Prelude
import Prim hiding (Type, Constraint)

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Bifunctor (bimap, lmap)
import Data.Bitraversable (bitraverse)
import Data.DateTime (DateTime)
import Data.Either (Either(..), note)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format, unformat)
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
import JSON (JSON)
import JSON as JSON
import JSON.ExtraCodecs (ToRecordCodec(..), fromArray, fromNullOrJust, fromOption, fromPropArray, fromRecordN, fromRequired, jsonTuple, toArray, toJArray, toJObject, toNull, toNullNothingOrJust, toOption, toOptionDefault, toOptionDefaultRename, toRecord, toRecordN, toRequired, toRequiredRename, toStatic, toString, tupleJSON, underIndex, underRequiredKey, withAttempts)
import JSON.Object as JO
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
  , modules :: Array DModule
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

packageJSON :: forall a. (a -> JSON) -> Package a -> JSON
packageJSON uploaderJSON = fromRecordN Package
  { packageMeta: fromRequired packageMetaJSON
  , version: fromRequired versionJSON
  , versionTag: fromRequired JSON.fromString
  , modules: fromRequired $ fromArray dModuleJSON
  , moduleMap: fromRequired $ (fromPropArray <<< map (bimap unwrap packageNameJSON) <<< Map.toUnfoldable)
  , resolvedDependencies: fromRequired $ (fromPropArray <<< map (bimap unwrap versionJSON))
  , github: fromRequired $ tupleJSON githubUserJSON githubRepoJSON
  , uploader: fromRequired uploaderJSON
  , compilerVersion: fromRequired versionJSON
  , tagTime: fromOption iso8601JSON
  }

jsonUploadedPackage :: Version -> JSON -> Either String UploadedPackage
jsonUploadedPackage minVersion = jsonPackage minVersion jsonNotYetKnown

jsonPackage :: forall a. Version -> (JSON -> Either String a) -> JSON -> Either String (Package a)
jsonPackage minimumVersion jsonUploader j = do
  jo <- toJObject j
  compilerVersion <- ((underRequiredKey "compilerVersion" jo jsonVersion) <|> (pure $ version 0 7 0 Nil Nil))
  when (compilerVersion < minimumVersion)
    (Left $ "Invalid compiler version: " <> showVersion compilerVersion :: Either String Unit)
  toRecordN Package
    { packageMeta: toRequired jsonPackageMeta
    , version: toRequired jsonVersion
    , versionTag: toRequired toString
    , tagTime: toOption jsonIso8601
    , modules: toRequired $ toArray jsonDModule
    , moduleMap: ToRecordCodec $ Right $ NonEmptyArray
        [ Tuple Nothing \key mbJ -> do
            x <- note ("under requird key " <> key <> ", ") mbJ
            lmap (append ("under requred key " <> key <> ", ")) do
              (JO.toUnfoldable <$> toJObject x)
                >>= flip Array.foldM Map.empty
                  ( \acc (Tuple k v) -> do
                      v' <- jsonPackageName v
                      pure $ Map.insert (ModuleName k) v' acc
                  )
        -- This is here to preserve backwards compatibility with compilers which used
        -- to generate a 'bookmarks' field in the JSON (i.e. up to 0.10.5). We should
        -- remove this after the next breaking change to the JSON.
        , Tuple (Just "bookmarks") \key mbJ -> do
            x <- note ("under requird key " <> key <> ", ") mbJ
            lmap (append ("under requred key " <> key <> ", ")) do
              arr <- flip toArray x $ jsonInPackage \j'' -> do
                jarr <- toJArray j''
                underIndex 0 jarr jsonModuleName
              pure $ Map.fromFoldable $ flip Array.mapMaybe arr case _ of
                Local _ -> Nothing
                FromDep pkgName mn -> Just $ Tuple mn pkgName
        ]
    , resolvedDependencies: toRequired $ toJObject >>> map JO.toUnfoldable >=> traverse (bitraverse (map (note "Invalid package name") parsePackageName) jsonVersion)
    , github: toRequired $ jsonTuple jsonGithubUser jsonGithubRepo
    , uploader: toRequired jsonUploader
    , compilerVersion: toStatic compilerVersion
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

iso8601JSON :: DateTime -> JSON
iso8601JSON = JSON.fromString <<< format encoder
  where
  encoder = List.fromFoldable $
    yyyy_mm_dd <> [ Placeholder "T" ] <> hh_mm <> _ss <> _sss <> [ Placeholder "Z" ]

jsonIso8601 :: JSON -> Either String DateTime
jsonIso8601 = toString >=> withAttempts (Array.cons primaryDecoder mainDecoders) \f j -> unformat f j
  where
  yyyy_mm_ddThh_mm = (Array.snoc yyyy_mm_dd (Placeholder "T")) <> hh_mm

  primaryDecoder :: Formatter
  primaryDecoder = List.fromFoldable $ yyyy_mm_ddThh_mm <> _ss <> [ Placeholder "+0000" ]

  mainDecoders :: Array Formatter
  mainDecoders = do
    sign <- [ [ Placeholder "+" ], [ Placeholder "-" ], [] ]
    _ss' <- [ _ss, [] ]
    _sss' <- [ _sss, [] ]
    pure $ List.fromFoldable $ sign <> yyyy_mm_ddThh_mm <> _ss' <> _sss' <> [ Placeholder "Z" ]

data NotYetKnown = NotYetKnown

derive instance Eq NotYetKnown
derive instance Ord NotYetKnown
derive instance Generic NotYetKnown _
instance Show NotYetKnown where
  show x = genericShow x

notYetKnownJSON :: NotYetKnown -> JSON
notYetKnownJSON = const JSON.null

jsonNotYetKnown :: JSON -> Either String NotYetKnown
jsonNotYetKnown = voidRight NotYetKnown <<< toNull

data ManifestError
  = BowerManifest BowerError
  | PursManifest PursJsonError

derive instance Eq ManifestError
derive instance Ord ManifestError
derive instance Generic ManifestError _
instance Show ManifestError where
  show x = genericShow x

newtype DModule = DModule
  { name :: ModuleName
  , comments :: Maybe String
  , declarations :: Array Declaration
  , reExports :: Array (Tuple (InPackage ModuleName) (Array Declaration))
  }

derive instance Eq DModule
derive instance Ord DModule
derive instance Newtype DModule _
derive instance Generic DModule _
instance Show DModule where
  show x = genericShow x

dModuleJSON :: DModule -> JSON
dModuleJSON = fromRecordN DModule
  { name: fromRequired $ unwrap >>> JSON.fromString
  , comments: fromOption JSON.fromString
  , declarations: fromRequired $ fromArray declarationJSON
  , reExports: fromRequired $ fromArray fromObj
  }
  where
  fromObj :: Tuple (InPackage ModuleName) (Array Declaration) -> JSON
  fromObj (Tuple mn decls) = fromPropArray
    [ Tuple "moduleName" $ inPackageJSON moduleNameJSON mn
    , Tuple "declarations" $ fromArray declarationJSON decls
    ]

jsonDModule :: JSON -> Either String DModule
jsonDModule = toRecordN DModule
  { name: toRequired $ coerce <<< toString
  , comments: toRequired $ toNullNothingOrJust toString
  , declarations: toRequired $ toArray jsonDeclaration
  , reExports: toRequired $ toArray jsonAsReExport
  }
  where
  jsonAsReExport :: JSON -> Either String (Tuple (InPackage ModuleName) (Array Declaration))
  jsonAsReExport =
    toRecord
      { moduleName: toRequired jsonAsRexportModuleName
      , declarations: toRequired $ toArray jsonDeclaration
      }
      >>> map \{ moduleName, declarations } -> Tuple moduleName declarations

  -- This is to preserve backwards compatibility with 0.10.3 and earlier versions
  -- of the compiler, where the modReExports field had the type
  -- [(P.ModuleName, [Declaration])]. This should eventually be removed,
  -- possibly at the same time as the next breaking change to this JSON format.
  jsonAsRexportModuleName :: JSON -> Either String (InPackage ModuleName)
  jsonAsRexportModuleName j = withAttempts
    [ jsonInPackage jsonModuleName
    , map Local <$> jsonModuleName
    ]
    (\f j' -> f j')
    j

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

declarationJSON :: Declaration -> JSON
declarationJSON = fromRecordN Declaration
  { title: fromRequired JSON.fromString
  , comments: fromRequired $ fromNullOrJust JSON.fromString
  , sourceSpan: fromRequired $ fromNullOrJust sourceSpanJSON
  , children: fromRequired $ fromArray childDeclarationJSON
  , info: fromRequired declarationInfoJSON
  , kind: fromOption kindInfoJSON
  }

jsonDeclaration :: JSON -> Either String Declaration
jsonDeclaration = toRecordN Declaration
  { title: toRequired toString
  , comments: toRequired $ toNullNothingOrJust toString
  , sourceSpan: toRequired $ toNullNothingOrJust jsonSourceSpan
  , children: toRequired $ toArray jsonChildDeclaration
  , info: toRequired jsonDeclarationInfo
  , kind: toOptionDefault Nothing $ toNullNothingOrJust jsonKindInfo
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

declarationInfoJSON :: DeclarationInfo -> JSON
declarationInfoJSON = case _ of
  ValueDeclaration ty -> fromPropArray
    [ Tuple "declType" $ JSON.fromString "value"
    , Tuple "type " $ typeJSON (const JSON.null) ty
    ]
  DataDeclaration ty args roles -> fromPropArray
    [ Tuple "declType" $ JSON.fromString "data"
    , Tuple "dataDeclType" $ dataDeclTypeJSON ty
    , Tuple "typeArguments" $ fromArray (tupleJSON JSON.fromString (fromNullOrJust (typeJSON (const JSON.null)))) args
    , Tuple "roles" $ fromArray roleJSON roles
    ]
  ExternDataDeclaration kind roles -> fromPropArray
    [ Tuple "declType" $ JSON.fromString "externData"
    , Tuple "kind" $ typeJSON (const JSON.null) kind
    , Tuple "roles" $ fromArray roleJSON roles
    ]
  TypeSynonymDeclaration args ty -> fromPropArray
    [ Tuple "declType" $ JSON.fromString "typeSynonym"
    , Tuple "arguments" $ fromArray (tupleJSON JSON.fromString (fromNullOrJust (typeJSON (const JSON.null)))) args
    , Tuple "type" $ typeJSON (const JSON.null) ty
    ]
  TypeClassDeclaration args super fundeps -> fromPropArray
    [ Tuple "declType" $ JSON.fromString "typeClass"
    , Tuple "arguments" $ fromArray (tupleJSON JSON.fromString (fromNullOrJust (typeJSON (const JSON.null)))) args
    , Tuple "superclasses" $ fromArray (constraintJSON (const JSON.null)) super
    , Tuple "fundeps" $ fromArray (tupleJSON (fromArray JSON.fromString) (fromArray JSON.fromString)) fundeps
    ]
  AliasDeclaration fixity alias -> fromPropArray
    [ Tuple "declType" $ JSON.fromString "alias"
    , Tuple "fixity" $ fixityJSON fixity
    , Tuple "alias" $ fixityAliasJSON alias
    ]

jsonDeclarationInfo :: JSON -> Either String DeclarationInfo
jsonDeclarationInfo j = do
  jo <- toJObject j
  declType <- underRequiredKey "declType" jo $ toString
  case declType of
    "value" ->
      ValueDeclaration
        <$> (underRequiredKey "type" jo $ jsonTypeUnit)
    "data" ->
      DataDeclaration
        <$> (underRequiredKey "dataDeclType" jo $ jsonDataDeclType)
        <*> (underRequiredKey "typeArguments" jo $ jsonTypeArguments)
        <*> ((underRequiredKey "roles" jo $ toArray jsonRole) <|> pure [])
    "externData" ->
      ExternDataDeclaration
        <$> (underRequiredKey "kind" jo $ jsonTypeUnit)
        <*> ((underRequiredKey "roles" jo $ toArray jsonRole) <|> pure [])
    "typeSynonym" ->
      TypeSynonymDeclaration
        <$> (underRequiredKey "arguments" jo $ jsonTypeArguments)
        <*> (underRequiredKey "type" jo $ jsonTypeUnit)
    "typeClass" ->
      TypeClassDeclaration
        <$> (underRequiredKey "arguments" jo $ jsonTypeArguments)
        <*> (underRequiredKey "superclasses" jo $ toArray jsonAsConstrantUnit)
        <*> ((underRequiredKey "fundeps" jo $ jsonFunDeps) <|> pure [])
    "alias" ->
      AliasDeclaration
        <$> (underRequiredKey "fixity" jo $ jsonFixity)
        <*> (underRequiredKey "alias" jo $ jsonFixityAlias)
    -- Backwards compat: kinds are extern data
    "kind" ->
      pure $ ExternDataDeclaration (void kindType) []
    str ->
      Left $ "Expected 'instance', 'dataConstructor', or 'typeClassMember' but got '" <> str <> "'."

jsonTypeArguments :: JSON -> Either String (Array (Tuple String (Maybe Type')))
jsonTypeArguments = toArray (jsonTuple toString (toNullNothingOrJust jsonTypeUnit))

jsonAsConstrantUnit :: JSON -> Either String Constraint'
jsonAsConstrantUnit = toRecordN Constraint
  { ann: toStatic unit
  , class: toRequiredRename "constraintClass" $ jsonQualified jsonProperName
  , kindArgs: toOptionDefaultRename "constraintKindArgs" [] $ toArray jsonTypeUnit
  , args: toRequiredRename "constraintArgs" $ toArray jsonTypeUnit
  , data: toStatic Nothing
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

kindInfoJSON :: KindInfo -> JSON
kindInfoJSON = fromRecordN KindInfo
  { keyword: fromRequired kindSignatureForJSON
  , kind: fromRequired $ typeJSON (const JSON.null)
  }

jsonKindInfo :: JSON -> Either String KindInfo
jsonKindInfo = toRecordN KindInfo
  { keyword: toRequired jsonKindSignatureFor
  , kind: toRequired jsonTypeUnit
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

childDeclarationJSON :: ChildDeclaration -> JSON
childDeclarationJSON = fromRecordN ChildDeclaration
  { title: fromRequired JSON.fromString
  , comments: fromRequired $ fromNullOrJust JSON.fromString
  , sourceSpan: fromRequired $ fromNullOrJust sourceSpanJSON
  , info: fromRequired childDeclarationInfoJSON
  }

jsonChildDeclaration :: JSON -> Either String ChildDeclaration
jsonChildDeclaration = toRecordN ChildDeclaration
  { title: toRequired toString
  , comments: toRequired $ toNullNothingOrJust toString
  , sourceSpan: toRequired $ toNullNothingOrJust jsonSourceSpan
  , info: toRequired jsonChildDeclarationInfo
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

childDeclarationInfoJSON :: ChildDeclarationInfo -> JSON
childDeclarationInfoJSON = case _ of
  ChildInstance deps ty -> fromPropArray
    [ Tuple "declType" $ JSON.fromString "instance"
    , Tuple "dependencies" $ fromArray (constraintJSON (const JSON.null)) deps
    , Tuple "type" $ typeJSON (const JSON.null) ty
    ]
  ChildDataConstructor args -> fromPropArray
    [ Tuple "declType" $ JSON.fromString "typeClassMember"
    , Tuple "arguments" $ fromArray (typeJSON (const JSON.null)) args
    ]
  ChildTypeClassMember ty -> fromPropArray
    [ Tuple "declType" $ JSON.fromString "typeClassMember"
    , Tuple "type" $ typeJSON (const JSON.null) ty
    ]

jsonChildDeclarationInfo :: JSON -> Either String ChildDeclarationInfo
jsonChildDeclarationInfo j = do
  jo <- toJObject j
  declType <- underRequiredKey "declType" jo toString
  case declType of
    "instance" ->
      ChildInstance
        <$> (underRequiredKey "dependencies" jo $ toArray jsonAsConstrantUnit)
        <*> (underRequiredKey "type" jo $ jsonTypeUnit)
    "dataConstructor" ->
      ChildDataConstructor
        <$> (underRequiredKey "arguments" jo $ toArray jsonTypeUnit)
    "typeClassMember" ->
      ChildTypeClassMember
        <$> (underRequiredKey "type" jo jsonTypeUnit)
    str ->
      Left $ "Expected 'instance', 'dataConstructor', or 'typeClassMember' but got '" <> str <> "'."

newtype GithubUser = GithubUser String

derive instance Eq GithubUser
derive instance Ord GithubUser
derive instance Newtype GithubUser _
derive instance Generic GithubUser _
instance Show GithubUser where
  show x = genericShow x

githubUserJSON :: GithubUser -> JSON
githubUserJSON = unwrap >>> JSON.fromString

jsonGithubUser :: JSON -> Either String GithubUser
jsonGithubUser = coerce <<< toString

newtype GithubRepo = GithubRepo String

derive instance Eq GithubRepo
derive instance Ord GithubRepo
derive instance Newtype GithubRepo _
derive instance Generic GithubRepo _
instance Show GithubRepo where
  show x = genericShow x

githubRepoJSON :: GithubRepo -> JSON
githubRepoJSON = unwrap >>> JSON.fromString

jsonGithubRepo :: JSON -> Either String GithubRepo
jsonGithubRepo = coerce <<< toString

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

inPackageJSON :: forall a. (a -> JSON) -> InPackage a -> JSON
inPackageJSON innerJSON = case _ of
  Local a -> fromPropArray
    [ Tuple "item" $ innerJSON a ]
  FromDep pkgName a -> fromPropArray
    [ Tuple "package" $ packageNameJSON pkgName
    , Tuple "item" $ innerJSON a
    ]

jsonInPackage :: forall a. (JSON -> Either String a) -> JSON -> Either String (InPackage a)
jsonInPackage jsonInner =
  toRecord
    { package: toOptionDefault Nothing $ toNullNothingOrJust jsonPackageName
    , item: toRequired jsonInner
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

jsonFunDeps :: JSON -> Either String (Array (Tuple (Array String) (Array String)))
jsonFunDeps = toArray (jsonTuple (toArray toString) (toArray toString))

versionJSON :: Version -> JSON
versionJSON = JSON.fromString <<< showVersion

jsonVersion :: JSON -> Either String Version
jsonVersion = toString >=> Version.parseVersion >>> lmap show
