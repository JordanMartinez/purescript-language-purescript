module GenConstants.Libs where

import Prelude hiding (mod)

import GenConstants.TH (AppM, asIdent, asPair, asString, cls, dty, mod, ntys, prefixWith, var, vars)

-- Note: this does not codegen `stRefValue` or any of the `EffectDictionaries` type/values.
codegenLibsModule :: Partial => AppM Unit
codegenLibsModule = do

  -- purescript-prelude

  mod "Control.Apply" do
    asIdent do asString do var "apply"

  mod "Control.Applicative" do
    asIdent do asPair do asString do var "pure"

  mod "Control.Bind" do
    asPair do
      asString do
        var "bind"

        cls "Discard"
        var "discard"

      var "discardUnit"

  mod "Control.Category" do
    asPair do
      asIdent do var "identity"

      var "categoryFn"

  mod "Control.Semigroupoid" do
    asPair do
      vars [ "compose", "composeFlipped" ]
      var "semigroupoidFn"

  mod "Data.Bounded" do
    asPair do
      vars [ "bottom", "top" ]
      var "boundedBoolean"

  mod "Data.Eq" do
    cls "Eq"
    asIdent do asPair do asString do var "eq"

    cls "Eq1"
    asIdent do asString do var "eq1"

    asPair do
      var "notEq"

      var "eqBoolean"
      var "eqChar"
      var "eqInt"
      var "eqNumber"
      var "eqString"

  mod "Data.EuclideanRing" do
    asPair do
      var "div"

      var "euclideanRingNumber"

  mod "Data.Function" do
    asIdent do
      prefixWith "function" do vars [ "apply", "applyFlipped" ]
      var "const"
      var "flip"

  mod "Data.Functor" do
    cls "Functor"
    asIdent do asString do var "map"

  mod "Data.Generic.Rep" do
    cls "Generic"
    asIdent do vars [ "from", "to" ]

    ntys [ "Argument", "Constructor", "NoArguments", "NoConstructors", "Product" ]
    dty "Sum" [ "Inl", "Inr" ]

  mod "Data.HeytingAlgebra" do
    asPair do
      asIdent do vars [ "conj", "disj", "not" ]

      var "heytingAlgebraBoolean"

  mod "Data.Monoid" do
    asIdent do var "mempty"

  mod "Data.Ord" do
    cls "Ord"
    asIdent do asString do var "compare"

    cls "Ord1"
    asIdent do asString do var "compare1"

    asPair do
      vars [ "greaterThan", "greaterThanOrEq", "lessThan", "lessThanOrEq" ]

      var "ordBoolean"
      var "ordChar"
      var "ordInt"
      var "ordNumber"
      var "ordString"

  mod "Data.Ordering" do
    dty "Ordering" [ "EQ", "GT", "LT" ]

  mod "Data.Reflectable" do
    cls "Reflectable"

  mod "Data.Ring" do
    asPair do
      asString do vars [ "negate", "sub" ]

      var "ringInt"
      var "ringNumber"

  mod "Data.Semigroup" do
    asPair do
      asIdent do var "append"

      var "semigroupString"

  mod "Data.Semiring" do
    asPair do
      vars [ "add", "mul", "one", "zero" ]

      var "semiringInt"
      var "semiringNumber"

  mod "Data.Symbol" do
    cls "IsSymbol"

  -- purescript-arrays

  mod "Data.Array" do
    asPair do var "unsafeIndex"

  -- purescript-bifunctors

  mod "Data.Bifunctor" do
    cls "Bifunctor"
    asIdent do asString do var "bimap"

    asIdent do vars [ "lmap", "rmap" ]

  -- purescript-contravariant

  mod "Data.Functor.Contravariant" do
    cls "Contravariant"
    asIdent do asString do var "cmap"

  -- purescript-eff

  mod "Control.Monad.Eff" mempty

  mod "Control.Monad.Eff.Uncurried" do
    asPair do vars [ "mkEffFn", "runEffFn" ]

  -- purescript-effect

  mod "Effect" mempty

  mod "Effect.Uncurried" do
    asPair do vars [ "mkEffectFn", "runEffectFn" ]

  -- purescript-foldable-traversable

  mod "Data.Bifoldable" do
    cls "Bifoldable"
    asIdent do asString do vars [ "bifoldMap", "bifoldl", "bifoldr" ]

  mod "Data.Bitraversable" do
    cls "Bitraversable"
    asString do asIdent do var "bitraverse"
    var "bisequence"

    asIdent do
      vars [ "ltraverse", "rtraverse" ]

  mod "Data.Foldable" do
    cls "Foldable"
    asIdent do asString do vars [ "foldMap", "foldl", "foldr" ]

  mod "Data.Traversable" do
    cls "Traversable"
    asString do asIdent do var "traverse"
    var "sequence"

  -- purescript-functions

  mod "Data.Function.Uncurried" do
    asPair do asString do vars [ "mkFn", "runFn" ]

  -- purescript-integers

  mod "Data.Int.Bits" do
    asPair do
      var "and"
      var "complement"
      var "or"
      var "shl"
      var "shr"
      var "xor"
      var "zshr"

  -- purescript-newtype

  mod "Data.Newtype" do
    cls "Newtype"

  -- purescript-partial

  mod "Partial.Unsafe" do
    asIdent do asPair do var "unsafePartial"

  -- purescript-profunctor

  mod "Data.Profunctor" do
    cls "Profunctor"
    asIdent do asString do var "dimap"

    asIdent do
      var "lcmap"
      prefixWith "profunctor" do var "rmap"

  -- purescript-st

  mod "Control.Monad.ST.Internal" do
    asPair do vars [ "modify", "new", "read", "run", "write" ]

  mod "Control.Monad.ST.Uncurried" do
    asPair do vars [ "mkSTFn", "runSTFn" ]

  -- purescript-unsafe-coerce

  mod "Unsafe.Coerce" do
    asPair do var "unsafeCoerce"

