module Language.PureScript.Types where

import Prelude
import Prim hiding (Type, Constraint)

import Control.Alt ((<|>))
import Data.Array (zipWith)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Foldable (class Foldable, and, fold)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import JSON (JSON)
import JSON as JSON
import JSON.ExtraCodecs (ToRecordCodec(..), fromArray, fromArray2, fromArray3, fromArray4, fromJoSingleton, fromNullOrJust, fromPropArray, fromRecord, fromRequiredRename, toArray, toArray2, toArray3, toArray4, toBoolean, toInt, toJObject, toNull, toNullNothingOrJust, toOptionDefaultRename, toRecord, toRequired, toRequiredRename, toString, underRequiredKey)
import Language.PureScript.AST.SourcePos (SourceAnn, nullSourceAnn)
import Language.PureScript.Constants.Prim as C
import Language.PureScript.Label (Label, jsonLabel, labelJSON)
import Language.PureScript.Names (ClassName, OpName, ProperName, Qualified, TypeName, TypeOpName, jsonOpName, jsonProperName, jsonQualified, opNameJSON, properNameJSON, qualifiedJSON)
import Language.PureScript.PSString (PSString, jsonPSString, psStringJSON)
import Safe.Coerce (coerce)

-- | An identifier for the scope of a skolem variable
newtype SkolemScope = SkolemScope Int

derive instance Eq SkolemScope
derive instance Ord SkolemScope
derive instance Newtype SkolemScope _
derive instance Generic SkolemScope _
instance Show SkolemScope where
  show x = genericShow x

skolemScopeJSON :: SkolemScope -> JSON
skolemScopeJSON = unwrap >>> JSON.fromInt

jsonSkolemScope :: JSON -> Either String SkolemScope
jsonSkolemScope = lmap (append "for SkolemScope, ") <<< coerce toInt

data WildcardData
  = HoleWildcard String
  | UnnamedWildcard
  | IgnoredWildcard

derive instance Eq WildcardData
derive instance Ord WildcardData
derive instance Generic WildcardData _
instance Show WildcardData where
  show x = genericShow x

wildcardDataJSON :: WildcardData -> JSON
wildcardDataJSON = case _ of
  HoleWildcard name -> JSON.fromString name
  UnnamedWildcard -> JSON.null
  IgnoredWildcard -> fromJoSingleton "ignored" (JSON.fromBoolean true)

jsonWildcardData :: JSON -> Either String WildcardData
jsonWildcardData j = holeWildcard <|> unnamedWildcard <|> ignoredWildcard
  where
  holeWildcard = HoleWildcard <$> toString j
  unnamedWildcard = UnnamedWildcard <$ toNull j
  ignoredWildcard = IgnoredWildcard <$ toJObject j

data TypeVarVisibility
  = TypeVarVisible
  | TypeVarInvisible

derive instance Eq TypeVarVisibility
derive instance Ord TypeVarVisibility
derive instance Generic TypeVarVisibility _
instance Show TypeVarVisibility where
  show x = genericShow x

typeVarVisibilityJSON :: TypeVarVisibility -> JSON
typeVarVisibilityJSON = JSON.fromString <<< case _ of
  TypeVarVisible -> "TypeVarVisible"
  TypeVarInvisible -> "TypeVarInvisible"

jsonTypeVarVisibility :: JSON -> Either String TypeVarVisibility
jsonTypeVarVisibility = toString >=> case _ of
  "TypeVarVisible" -> pure TypeVarVisible
  "TypeVarInvisible" -> pure TypeVarInvisible
  str -> Left $ "Expected 'TypeVarVisible' or 'TypeVarInvisible' but got '" <> str <> "'."

type SourceType = Type SourceAnn

data Type a
  = TUnknown a Int
  | TypeVar a String
  | TypeLevelString a PSString
  | TypeLevelInt a Int
  | TypeWildcard a WildcardData
  | TypeConstructor a (Qualified (ProperName TypeName))
  | TypeOp a (Qualified (OpName TypeOpName))
  | TypeApp a (Type a) (Type a)
  | KindApp a (Type a) (Type a)
  | ForAll a TypeVarVisibility String (Maybe (Type a)) (Type a) (Maybe SkolemScope)
  | ConstrainedType a (Constraint a) (Type a)
  | Skolem a String (Maybe (Type a)) Int SkolemScope
  | REmpty a
  | RCons a Label (Type a) (Type a)
  | KindedType a (Type a) (Type a)
  | BinaryNoParensType a (Type a) (Type a) (Type a)
  | ParensInType a (Type a)

derive instance Functor Type
derive instance Foldable Type
derive instance Traversable Type
derive instance Generic (Type a) _
instance Show a => Show (Type a) where
  show x = genericShow x

srcTypeConstructor :: Qualified (ProperName TypeName) -> SourceType
srcTypeConstructor = TypeConstructor nullSourceAnn

typeJSON :: forall a. (a -> JSON) -> Type a -> JSON
typeJSON annJSON ty =
  case ty of
    TUnknown a b ->
      variant "TUnknown" a $ JSON.fromInt b
    TypeVar a b ->
      variant "TypeVar" a $ JSON.fromString b
    TypeLevelString a b ->
      variant "TypeLevelString" a $ psStringJSON b
    TypeLevelInt a b ->
      variant "TypeLevelInt" a $ JSON.fromInt b
    TypeWildcard a b ->
      variant "TypeWildcard" a $ wildcardDataJSON b
    TypeConstructor a b ->
      variant "TypeConstructor" a $ qualifiedJSON properNameJSON b
    TypeOp a b ->
      variant "TypeOp" a $ qualifiedJSON opNameJSON b
    TypeApp a b c ->
      variant "TypeApp" a $ fromArray2 (go b) (go c)
    KindApp a b c ->
      variant "KindApp" a $ fromArray2 (go b) (go c)
    ForAll a b c d e f ->
      variant "ForAll" a $ fromPropArray
        [ Tuple "visibility" $ typeVarVisibilityJSON b
        , Tuple "identifier" $ JSON.fromString c
        , Tuple "kind" $ fromNullOrJust go d
        , Tuple "type" $ go e
        , Tuple "skolem" $ fromNullOrJust skolemScopeJSON f
        ]
    ConstrainedType a b c ->
      variant "ConstrainedType" a $ fromArray2 (constraintJSON annJSON b) (go c)
    Skolem a b c d e ->
      variant "Skolem" a $ fromArray4 (JSON.fromString b) (fromNullOrJust go c) (JSON.fromInt d) (skolemScopeJSON e)
    REmpty a ->
      nullary "REmpty" a
    RCons a b c d ->
      variant "RCons" a $ fromArray3 (labelJSON b) (go c) (go d)
    KindedType a b c ->
      variant "KindedType" a $ fromArray2 (go b) (go c)
    BinaryNoParensType a b c d ->
      variant "BinaryNoParensType" a $ fromArray3 (go b) (go c) (go d)
    ParensInType a b ->
      variant "ParensInType" a (go b)
  where
  go :: Type a -> JSON
  go = typeJSON annJSON

  variant :: String -> a -> JSON -> JSON
  variant tag ann contents = fromPropArray
    [ Tuple "tag" $ JSON.fromString tag
    , Tuple "annotation" $ annJSON ann
    , Tuple "contents" $ contents
    ]

  nullary :: String -> a -> JSON
  nullary tag ann = fromPropArray
    [ Tuple "tag" $ JSON.fromString tag
    , Tuple "annotation" $ annJSON ann
    ]

jsonSourceType :: (JSON -> Either String SourceAnn) -> JSON -> Either String (Type SourceAnn)
jsonSourceType jsonAnn = jsonType' (pure nullSourceAnn) jsonAnn

jsonTypeUnit :: JSON -> Either String (Type Unit)
jsonTypeUnit = jsonType' (pure unit) toNull

jsonType' :: forall a. Either String a -> (JSON -> Either String a) -> JSON -> Either String (Type a)
jsonType' defaultAnn jsonAnn j = do
  o <- toJObject j
  tag <- underRequiredKey "tag" o toString
  a <- (underRequiredKey "annotation" o jsonAnn) <|> defaultAnn
  let
    contents :: forall x. (JSON -> Either String x) -> Either String x
    contents = underRequiredKey "contents" o
  case tag of
    "TUnknown" ->
      TUnknown a <$> (contents toInt)
    "TypeVar" ->
      TypeVar a <$> (contents toString)
    "TypeLevelString" ->
      TypeLevelString a <$> (contents jsonPSString)
    "TypeLevelInt" ->
      TypeLevelInt a <$> (contents toInt)
    "TypeWildcard" -> do
      TypeWildcard a <$> ((contents jsonWildcardData) <|> pure UnnamedWildcard)
    "TypeConstructor" ->
      TypeConstructor a <$> (contents $ jsonQualified jsonProperName)
    "TypeOp" ->
      TypeOp a <$> (contents $ jsonQualified jsonOpName)
    "TypeApp" -> do
      contents $ toArray2 go go (TypeApp a)
    "KindApp" -> do
      contents $ toArray2 go go (KindApp a)
    "ForAll" -> do
      let
        asObject = do
          { v, i, k, t, s } <- contents $ toRecord
            { v: toRequiredRename "visibility" jsonTypeVarVisibility
            , i: toRequiredRename "identifier" toString
            , k: toOptionDefaultRename "kind" Nothing $ toNullNothingOrJust go
            , t: toRequiredRename "type" go
            , s: toRequiredRename "skolem" $ toNullNothingOrJust jsonSkolemScope
            }
          pure $ ForAll a v i k t s

        withoutMbKind = do
          contents $ toArray3 toString go (toNullNothingOrJust jsonSkolemScope) \i t s ->
            ForAll a TypeVarInvisible i Nothing t s

        withMbKind = do
          contents $ toArray4 toString (toNullNothingOrJust go) go (toNullNothingOrJust jsonSkolemScope) \i k t s ->
            ForAll a TypeVarInvisible i k t s
      asObject <|> withMbKind <|> withoutMbKind
    "ConstrainedType" -> do
      contents $ toArray2 (jsonConstraint' defaultAnn jsonAnn) go (ConstrainedType a)
    "Skolem" -> do
      contents $ toArray4 toString (toNullNothingOrJust go) toInt jsonSkolemScope (Skolem a)
    "REmpty" ->
      pure $ REmpty a
    "RCons" -> do
      contents $ toArray3 (jsonLabel) go go (RCons a)
    "KindedType" -> do
      contents $ toArray2 go go (KindedType a)
    "BinaryNoParensType" -> do
      contents $ toArray3 go go go (BinaryNoParensType a)
    "ParensInType" -> do
      ParensInType a <$> (contents go)
    -- Backwards compatibility for kinds
    -- See https://github.com/purescript/purescript/pull/3779/files#diff-870a1f93bcc1630036804836b97cff8471bfaeb781b70545aea51343786085a5
    "KUnknown" ->
      TUnknown a <$> (contents toInt)
    "Row" ->
      TypeApp a (TypeConstructor a C.tyRow) <$> (contents go)
    "FunKind" -> do
      contents $ toArray2 go go \b c ->
        TypeApp a (TypeApp a (TypeConstructor a C.tyFunction) b) c
    "NamedKind" ->
      TypeConstructor a <$> (contents $ jsonQualified jsonProperName)
    str ->
      Left $ "Unexpected value for `declType`: " <> str
  where
  go :: JSON -> Either String (Type a)
  go = jsonType' defaultAnn jsonAnn

-- | Additional data relevant to type class constraints
data ConstraintData = PartialConstraintData (Array (Array String)) Boolean

derive instance Eq ConstraintData
derive instance Ord ConstraintData
derive instance Generic ConstraintData _
instance Show ConstraintData where
  show x = genericShow x

constraintDataJSON :: ConstraintData -> JSON
constraintDataJSON = case _ of
  PartialConstraintData bs trunc ->
    fromJoSingleton "contents" $ fromArray2
      (fromArray (fromArray JSON.fromString) bs)
      (JSON.fromBoolean trunc)

jsonConstraintData :: JSON -> Either String ConstraintData
jsonConstraintData = map _.contents <<< toRecord
  { contents: toRequired $ toArray2 (toArray (toArray toString)) toBoolean PartialConstraintData
  }

type SourceConstraint = Constraint SourceAnn

-- | A typeclass constraint
newtype Constraint a = Constraint
  { ann :: a
  , class :: Qualified (ProperName ClassName)
  , kindArgs :: Array (Type a)
  , args :: Array (Type a)
  , "data" :: Maybe ConstraintData
  }

derive instance Newtype (Constraint a) _
derive instance Generic (Constraint a) _
derive instance Functor Constraint
derive instance Foldable Constraint
derive instance Traversable Constraint
instance Show a => Show (Constraint a) where
  show x = genericShow x

constraintJSON :: forall a. (a -> JSON) -> Constraint a -> JSON
constraintJSON annJSON = unwrap >>>
  fromRecord
    { ann: fromRequiredRename "constraintAnn" annJSON
    , class: fromRequiredRename "constraintClass" $ qualifiedJSON properNameJSON
    , kindArgs: fromRequiredRename "constraintKindArgs" $ fromArray $ typeJSON annJSON
    , args: fromRequiredRename "constraintArgs" $ fromArray $ typeJSON annJSON
    , data: fromRequiredRename "constraintData" $ fromNullOrJust constraintDataJSON
    }

jsonSourceConstraint :: (JSON -> Either String SourceAnn) -> JSON -> Either String (Constraint SourceAnn)
jsonSourceConstraint jsonAnn = jsonConstraint' (pure nullSourceAnn) jsonAnn

jsonConstraintUnit :: (JSON -> Either String Unit) -> JSON -> Either String (Constraint Unit)
jsonConstraintUnit jsonAnn = jsonConstraint' (pure unit) jsonAnn

jsonConstraint' :: forall a. Either String a -> (JSON -> Either String a) -> JSON -> Either String (Constraint a)
jsonConstraint' defaultAnn jsonAnn = map Constraint <<< toRecord
  { ann: ToRecordCodec $ Right $ NEA.singleton $ Tuple (Just "constraintAnn") $ \key mbValue ->
      ((note ("missing key " <> key <> ", ") mbValue) >>= jsonAnn) <|> defaultAnn
  , class: toRequiredRename "constraintClass" $ jsonQualified jsonProperName
  , kindArgs: toOptionDefaultRename "constraintKindArgs" [] $ toArray $ jsonType' defaultAnn jsonAnn
  , args: toRequiredRename "constraintArgs" $ toArray $ jsonType' defaultAnn jsonAnn
  , data: toRequiredRename "constraintData" $ toNullNothingOrJust jsonConstraintData
  }

newtype RowListItem a = RowListItem
  { ann :: a
  , label :: Label
  , type :: Type a
  }

derive instance Eq a => Eq (RowListItem a)
derive instance Ord a => Ord (RowListItem a)
derive instance Newtype (RowListItem a) _
derive instance Generic (RowListItem a) _
instance Show a => Show (RowListItem a) where
  show x = genericShow x

derive instance Functor RowListItem
derive instance Foldable RowListItem
derive instance Traversable RowListItem

instance Eq (Type a) where
  eq a b = eqType a b

instance Ord (Type a) where
  compare a b = compareType a b

eqType :: forall a b. Type a -> Type b -> Boolean
eqType = case _, _ of
  TUnknown _ a, TUnknown _ a' ->
    a == a'
  TypeVar _ a, TypeVar _ a' ->
    a == a'
  TypeLevelString _ a, TypeLevelString _ a' ->
    a == a'
  TypeLevelInt _ a, TypeLevelInt _ a' ->
    a == a'
  TypeWildcard _ a, TypeWildcard _ a' ->
    a == a'
  TypeConstructor _ a, TypeConstructor _ a' ->
    a == a'
  TypeOp _ a, TypeOp _ a' ->
    a == a'
  TypeApp _ a b, TypeApp _ a' b' ->
    eqType a a' && eqType b b'
  KindApp _ a b, KindApp _ a' b' ->
    eqType a a' && eqType b b'
  ForAll _ _ a b c d, ForAll _ _ a' b' c' d' ->
    a == a' && eqMaybeType b b' && eqType c c' && d == d'
  ConstrainedType _ a b, ConstrainedType _ a' b' ->
    eqConstraint a a' && eqType b b'
  Skolem _ a b c d, Skolem _ a' b' c' d' ->
    a == a' && eqMaybeType b b' && c == c' && d == d'
  REmpty _, REmpty _ ->
    true
  RCons _ a b c, RCons _ a' b' c' ->
    a == a' && eqType b b' && eqType c c'
  KindedType _ a b, KindedType _ a' b' ->
    eqType a a' && eqType b b'
  BinaryNoParensType _ a b c, BinaryNoParensType _ a' b' c' ->
    eqType a a' && eqType b b' && eqType c c'
  ParensInType _ a, ParensInType _ a' ->
    eqType a a'
  _, _ ->
    false

eqMaybeType :: forall a b. Maybe (Type a) -> Maybe (Type b) -> Boolean
eqMaybeType = case _, _ of
  Just a, Just b -> eqType a b
  Nothing, Nothing -> true
  _, _ -> false

compareType :: forall a b. Type a -> Type b -> Ordering
compareType = case _, _ of
  TUnknown _ a, TUnknown _ a' ->
    compare a a'
  TypeVar _ a, TypeVar _ a' ->
    compare a a'
  TypeLevelString _ a, TypeLevelString _ a' ->
    compare a a'
  TypeLevelInt _ a, TypeLevelInt _ a' ->
    compare a a'
  TypeWildcard _ a, TypeWildcard _ a' ->
    compare a a'
  TypeConstructor _ a, TypeConstructor _ a' ->
    compare a a'
  TypeOp _ a, TypeOp _ a' ->
    compare a a'
  TypeApp _ a b, TypeApp _ a' b' ->
    compareType a a' <> compareType b b'
  KindApp _ a b, KindApp _ a' b' ->
    compareType a a' <> compareType b b'
  ForAll _ _ a b c d, ForAll _ _ a' b' c' d' ->
    compare a a' <> compareMaybeType b b' <> compareType c c' <> compare d d'
  ConstrainedType _ a b, ConstrainedType _ a' b' ->
    compareConstraint a a' <> compareType b b'
  Skolem _ a b c d, Skolem _ a' b' c' d' ->
    compare a a' <> compareMaybeType b b' <> compare c c' <> compare d d'
  REmpty _, REmpty _ ->
    EQ
  RCons _ a b c, RCons _ a' b' c' ->
    compare a a' <> compareType b b' <> compareType c c'
  KindedType _ a b, KindedType _ a' b' ->
    compareType a a' <> compareType b b'
  BinaryNoParensType _ a b c, BinaryNoParensType _ a' b' c' ->
    compareType a a' <> compareType b b' <> compareType c c'
  ParensInType _ a, ParensInType _ a' ->
    compareType a a'
  typ, typ' ->
    compare (orderOf typ) (orderOf typ')
  where
  orderOf :: forall x. Type x -> Int
  orderOf = case _ of
    TUnknown _ _ -> 0
    TypeVar _ _ -> 1
    TypeLevelString _ _ -> 2
    TypeLevelInt _ _ -> 3
    TypeWildcard _ _ -> 4
    TypeConstructor _ _ -> 5
    TypeOp _ _ -> 6
    TypeApp _ _ _ -> 7
    KindApp _ _ _ -> 8
    ForAll _ _ _ _ _ _ -> 9
    ConstrainedType _ _ _ -> 10
    Skolem _ _ _ _ _ -> 11
    REmpty _ -> 12
    RCons _ _ _ _ -> 13
    KindedType _ _ _ -> 14
    BinaryNoParensType _ _ _ _ -> 15
    ParensInType _ _ -> 16

compareMaybeType :: forall a b. Maybe (Type a) -> Maybe (Type b) -> Ordering
compareMaybeType = case _, _ of
  Just a, Just b -> compareType a b
  Nothing, Nothing -> EQ
  Nothing, _ -> LT
  _, _ -> GT

instance Eq (Constraint a) where
  eq a b = eqConstraint a b

instance Ord (Constraint a) where
  compare a b = compareConstraint a b

eqConstraint :: forall a b. Constraint a -> Constraint b -> Boolean
eqConstraint = case _, _ of
  Constraint l, Constraint r ->
    l.class == r.class
      && and (zipWith eqType l.kindArgs r.kindArgs)
      && and (zipWith eqType l.args r.args)
      && l.data == r.data

compareConstraint :: forall a b. Constraint a -> Constraint b -> Ordering
compareConstraint = case _, _ of
  Constraint l, Constraint r ->
    compare l.class r.class
      <> fold (zipWith compareType l.kindArgs r.kindArgs)
      <> fold (zipWith compareType l.args r.args)
      <> compare l.data r.data
