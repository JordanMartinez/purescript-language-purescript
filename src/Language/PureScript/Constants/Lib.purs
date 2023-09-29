------------------------------------
-- This module is code generated. --
--          DO NOT EDIT!          --
------------------------------------
module Language.PureScript.Constants.Lib where

import Data.Tuple (Tuple(..))
import Language.PureScript.Names (ConstructorName, TypeName, Ident(..), ModuleName(..), ProperName(..), Qualified(..), QualifiedBy(..))

m_Control_Apply :: ModuleName
m_Control_Apply = ModuleName "Control.Apply"

s_apply :: String
s_apply = "apply"

i_apply :: Qualified Ident
i_apply = Qualified (ByModuleName m_Control_Apply) (Ident "apply")

m_Control_Applicative :: ModuleName
m_Control_Applicative = ModuleName "Control.Applicative"

s_pure :: String
s_pure = "pure"

p_pure :: Tuple ModuleName String
p_pure = Tuple m_Control_Applicative "pure"

i_pure :: Qualified Ident
i_pure = Qualified (ByModuleName m_Control_Applicative) (Ident "pure")

m_Control_Bind :: ModuleName
m_Control_Bind = ModuleName "Control.Bind"

s_bind :: String
s_bind = "bind"

p_bind :: Tuple ModuleName String
p_bind = Tuple m_Control_Bind "bind"

clsDiscard :: Qualified (ProperName TypeName)
clsDiscard = Qualified (ByModuleName m_Control_Bind) (ProperName "Discard")

s_discard :: String
s_discard = "discard"

p_discard :: Tuple ModuleName String
p_discard = Tuple m_Control_Bind "discard"

p_discardUnit :: Tuple ModuleName String
p_discardUnit = Tuple m_Control_Bind "discardUnit"

m_Control_Category :: ModuleName
m_Control_Category = ModuleName "Control.Category"

i_identity :: Qualified Ident
i_identity = Qualified (ByModuleName m_Control_Category) (Ident "identity")

p_identity :: Tuple ModuleName String
p_identity = Tuple m_Control_Category "identity"

p_categoryFn :: Tuple ModuleName String
p_categoryFn = Tuple m_Control_Category "categoryFn"

m_Control_Semigroupoid :: ModuleName
m_Control_Semigroupoid = ModuleName "Control.Semigroupoid"

p_compose :: Tuple ModuleName String
p_compose = Tuple m_Control_Semigroupoid "compose"

p_composeFlipped :: Tuple ModuleName String
p_composeFlipped = Tuple m_Control_Semigroupoid "composeFlipped"

p_semigroupoidFn :: Tuple ModuleName String
p_semigroupoidFn = Tuple m_Control_Semigroupoid "semigroupoidFn"

m_Data_Bounded :: ModuleName
m_Data_Bounded = ModuleName "Data.Bounded"

p_bottom :: Tuple ModuleName String
p_bottom = Tuple m_Data_Bounded "bottom"

p_top :: Tuple ModuleName String
p_top = Tuple m_Data_Bounded "top"

p_boundedBoolean :: Tuple ModuleName String
p_boundedBoolean = Tuple m_Data_Bounded "boundedBoolean"

m_Data_Eq :: ModuleName
m_Data_Eq = ModuleName "Data.Eq"

clsEq :: Qualified (ProperName TypeName)
clsEq = Qualified (ByModuleName m_Data_Eq) (ProperName "Eq")

s_eq :: String
s_eq = "eq"

p_eq :: Tuple ModuleName String
p_eq = Tuple m_Data_Eq "eq"

i_eq :: Qualified Ident
i_eq = Qualified (ByModuleName m_Data_Eq) (Ident "eq")

clsEq1 :: Qualified (ProperName TypeName)
clsEq1 = Qualified (ByModuleName m_Data_Eq) (ProperName "Eq1")

s_eq1 :: String
s_eq1 = "eq1"

i_eq1 :: Qualified Ident
i_eq1 = Qualified (ByModuleName m_Data_Eq) (Ident "eq1")

p_notEq :: Tuple ModuleName String
p_notEq = Tuple m_Data_Eq "notEq"

p_eqBoolean :: Tuple ModuleName String
p_eqBoolean = Tuple m_Data_Eq "eqBoolean"

p_eqChar :: Tuple ModuleName String
p_eqChar = Tuple m_Data_Eq "eqChar"

p_eqInt :: Tuple ModuleName String
p_eqInt = Tuple m_Data_Eq "eqInt"

p_eqNumber :: Tuple ModuleName String
p_eqNumber = Tuple m_Data_Eq "eqNumber"

p_eqString :: Tuple ModuleName String
p_eqString = Tuple m_Data_Eq "eqString"

m_Data_EuclideanRing :: ModuleName
m_Data_EuclideanRing = ModuleName "Data.EuclideanRing"

p_div :: Tuple ModuleName String
p_div = Tuple m_Data_EuclideanRing "div"

p_euclideanRingNumber :: Tuple ModuleName String
p_euclideanRingNumber = Tuple m_Data_EuclideanRing "euclideanRingNumber"

m_Data_Function :: ModuleName
m_Data_Function = ModuleName "Data.Function"

i_functionApply :: Qualified Ident
i_functionApply = Qualified (ByModuleName m_Data_Function) (Ident "apply")

i_functionApplyFlipped :: Qualified Ident
i_functionApplyFlipped = Qualified (ByModuleName m_Data_Function) (Ident "applyFlipped")

i_const :: Qualified Ident
i_const = Qualified (ByModuleName m_Data_Function) (Ident "const")

i_flip :: Qualified Ident
i_flip = Qualified (ByModuleName m_Data_Function) (Ident "flip")

m_Data_Functor :: ModuleName
m_Data_Functor = ModuleName "Data.Functor"

clsFunctor :: Qualified (ProperName TypeName)
clsFunctor = Qualified (ByModuleName m_Data_Functor) (ProperName "Functor")

s_map :: String
s_map = "map"

i_map :: Qualified Ident
i_map = Qualified (ByModuleName m_Data_Functor) (Ident "map")

m_Data_Generic_Rep :: ModuleName
m_Data_Generic_Rep = ModuleName "Data.Generic.Rep"

clsGeneric :: Qualified (ProperName TypeName)
clsGeneric = Qualified (ByModuleName m_Data_Generic_Rep) (ProperName "Generic")

i_from :: Qualified Ident
i_from = Qualified (ByModuleName m_Data_Generic_Rep) (Ident "from")

i_to :: Qualified Ident
i_to = Qualified (ByModuleName m_Data_Generic_Rep) (Ident "to")

tyArgument :: Qualified (ProperName TypeName)
tyArgument = Qualified (ByModuleName m_Data_Generic_Rep) (ProperName "Argument")

ctorArgument :: Qualified (ProperName ConstructorName)
ctorArgument = Qualified (ByModuleName m_Data_Generic_Rep) (ProperName "Argument")

tyConstructor :: Qualified (ProperName TypeName)
tyConstructor = Qualified (ByModuleName m_Data_Generic_Rep) (ProperName "Constructor")

ctorConstructor :: Qualified (ProperName ConstructorName)
ctorConstructor = Qualified (ByModuleName m_Data_Generic_Rep) (ProperName "Constructor")

tyNoArguments :: Qualified (ProperName TypeName)
tyNoArguments = Qualified (ByModuleName m_Data_Generic_Rep) (ProperName "NoArguments")

ctorNoArguments :: Qualified (ProperName ConstructorName)
ctorNoArguments = Qualified (ByModuleName m_Data_Generic_Rep) (ProperName "NoArguments")

tyNoConstructors :: Qualified (ProperName TypeName)
tyNoConstructors = Qualified (ByModuleName m_Data_Generic_Rep) (ProperName "NoConstructors")

ctorNoConstructors :: Qualified (ProperName ConstructorName)
ctorNoConstructors = Qualified (ByModuleName m_Data_Generic_Rep) (ProperName "NoConstructors")

tyProduct :: Qualified (ProperName TypeName)
tyProduct = Qualified (ByModuleName m_Data_Generic_Rep) (ProperName "Product")

ctorProduct :: Qualified (ProperName ConstructorName)
ctorProduct = Qualified (ByModuleName m_Data_Generic_Rep) (ProperName "Product")

tySum :: Qualified (ProperName TypeName)
tySum = Qualified (ByModuleName m_Data_Generic_Rep) (ProperName "Sum")

ctorInl :: Qualified (ProperName ConstructorName)
ctorInl = Qualified (ByModuleName m_Data_Generic_Rep) (ProperName "Inl")

ctorInr :: Qualified (ProperName ConstructorName)
ctorInr = Qualified (ByModuleName m_Data_Generic_Rep) (ProperName "Inr")

m_Data_HeytingAlgebra :: ModuleName
m_Data_HeytingAlgebra = ModuleName "Data.HeytingAlgebra"

i_conj :: Qualified Ident
i_conj = Qualified (ByModuleName m_Data_HeytingAlgebra) (Ident "conj")

p_conj :: Tuple ModuleName String
p_conj = Tuple m_Data_HeytingAlgebra "conj"

i_disj :: Qualified Ident
i_disj = Qualified (ByModuleName m_Data_HeytingAlgebra) (Ident "disj")

p_disj :: Tuple ModuleName String
p_disj = Tuple m_Data_HeytingAlgebra "disj"

i_not :: Qualified Ident
i_not = Qualified (ByModuleName m_Data_HeytingAlgebra) (Ident "not")

p_not :: Tuple ModuleName String
p_not = Tuple m_Data_HeytingAlgebra "not"

p_heytingAlgebraBoolean :: Tuple ModuleName String
p_heytingAlgebraBoolean = Tuple m_Data_HeytingAlgebra "heytingAlgebraBoolean"

m_Data_Monoid :: ModuleName
m_Data_Monoid = ModuleName "Data.Monoid"

i_mempty :: Qualified Ident
i_mempty = Qualified (ByModuleName m_Data_Monoid) (Ident "mempty")

m_Data_Ord :: ModuleName
m_Data_Ord = ModuleName "Data.Ord"

clsOrd :: Qualified (ProperName TypeName)
clsOrd = Qualified (ByModuleName m_Data_Ord) (ProperName "Ord")

s_compare :: String
s_compare = "compare"

i_compare :: Qualified Ident
i_compare = Qualified (ByModuleName m_Data_Ord) (Ident "compare")

clsOrd1 :: Qualified (ProperName TypeName)
clsOrd1 = Qualified (ByModuleName m_Data_Ord) (ProperName "Ord1")

s_compare1 :: String
s_compare1 = "compare1"

i_compare1 :: Qualified Ident
i_compare1 = Qualified (ByModuleName m_Data_Ord) (Ident "compare1")

p_greaterThan :: Tuple ModuleName String
p_greaterThan = Tuple m_Data_Ord "greaterThan"

p_greaterThanOrEq :: Tuple ModuleName String
p_greaterThanOrEq = Tuple m_Data_Ord "greaterThanOrEq"

p_lessThan :: Tuple ModuleName String
p_lessThan = Tuple m_Data_Ord "lessThan"

p_lessThanOrEq :: Tuple ModuleName String
p_lessThanOrEq = Tuple m_Data_Ord "lessThanOrEq"

p_ordBoolean :: Tuple ModuleName String
p_ordBoolean = Tuple m_Data_Ord "ordBoolean"

p_ordChar :: Tuple ModuleName String
p_ordChar = Tuple m_Data_Ord "ordChar"

p_ordInt :: Tuple ModuleName String
p_ordInt = Tuple m_Data_Ord "ordInt"

p_ordNumber :: Tuple ModuleName String
p_ordNumber = Tuple m_Data_Ord "ordNumber"

p_ordString :: Tuple ModuleName String
p_ordString = Tuple m_Data_Ord "ordString"

m_Data_Ordering :: ModuleName
m_Data_Ordering = ModuleName "Data.Ordering"

tyOrdering :: Qualified (ProperName TypeName)
tyOrdering = Qualified (ByModuleName m_Data_Ordering) (ProperName "Ordering")

ctorEQ :: Qualified (ProperName ConstructorName)
ctorEQ = Qualified (ByModuleName m_Data_Ordering) (ProperName "EQ")

ctorGT :: Qualified (ProperName ConstructorName)
ctorGT = Qualified (ByModuleName m_Data_Ordering) (ProperName "GT")

ctorLT :: Qualified (ProperName ConstructorName)
ctorLT = Qualified (ByModuleName m_Data_Ordering) (ProperName "LT")

m_Data_Reflectable :: ModuleName
m_Data_Reflectable = ModuleName "Data.Reflectable"

clsReflectable :: Qualified (ProperName TypeName)
clsReflectable = Qualified (ByModuleName m_Data_Reflectable) (ProperName "Reflectable")

m_Data_Ring :: ModuleName
m_Data_Ring = ModuleName "Data.Ring"

s_negate :: String
s_negate = "negate"

p_negate :: Tuple ModuleName String
p_negate = Tuple m_Data_Ring "negate"

s_sub :: String
s_sub = "sub"

p_sub :: Tuple ModuleName String
p_sub = Tuple m_Data_Ring "sub"

p_ringInt :: Tuple ModuleName String
p_ringInt = Tuple m_Data_Ring "ringInt"

p_ringNumber :: Tuple ModuleName String
p_ringNumber = Tuple m_Data_Ring "ringNumber"

m_Data_Semigroup :: ModuleName
m_Data_Semigroup = ModuleName "Data.Semigroup"

i_append :: Qualified Ident
i_append = Qualified (ByModuleName m_Data_Semigroup) (Ident "append")

p_append :: Tuple ModuleName String
p_append = Tuple m_Data_Semigroup "append"

p_semigroupString :: Tuple ModuleName String
p_semigroupString = Tuple m_Data_Semigroup "semigroupString"

m_Data_Semiring :: ModuleName
m_Data_Semiring = ModuleName "Data.Semiring"

p_add :: Tuple ModuleName String
p_add = Tuple m_Data_Semiring "add"

p_mul :: Tuple ModuleName String
p_mul = Tuple m_Data_Semiring "mul"

p_one :: Tuple ModuleName String
p_one = Tuple m_Data_Semiring "one"

p_zero :: Tuple ModuleName String
p_zero = Tuple m_Data_Semiring "zero"

p_semiringInt :: Tuple ModuleName String
p_semiringInt = Tuple m_Data_Semiring "semiringInt"

p_semiringNumber :: Tuple ModuleName String
p_semiringNumber = Tuple m_Data_Semiring "semiringNumber"

m_Data_Symbol :: ModuleName
m_Data_Symbol = ModuleName "Data.Symbol"

clsIsSymbol :: Qualified (ProperName TypeName)
clsIsSymbol = Qualified (ByModuleName m_Data_Symbol) (ProperName "IsSymbol")

m_Data_Array :: ModuleName
m_Data_Array = ModuleName "Data.Array"

p_unsafeIndex :: Tuple ModuleName String
p_unsafeIndex = Tuple m_Data_Array "unsafeIndex"

m_Data_Bifunctor :: ModuleName
m_Data_Bifunctor = ModuleName "Data.Bifunctor"

clsBifunctor :: Qualified (ProperName TypeName)
clsBifunctor = Qualified (ByModuleName m_Data_Bifunctor) (ProperName "Bifunctor")

s_bimap :: String
s_bimap = "bimap"

i_bimap :: Qualified Ident
i_bimap = Qualified (ByModuleName m_Data_Bifunctor) (Ident "bimap")

i_lmap :: Qualified Ident
i_lmap = Qualified (ByModuleName m_Data_Bifunctor) (Ident "lmap")

i_rmap :: Qualified Ident
i_rmap = Qualified (ByModuleName m_Data_Bifunctor) (Ident "rmap")

m_Data_Functor_Contravariant :: ModuleName
m_Data_Functor_Contravariant = ModuleName "Data.Functor.Contravariant"

clsContravariant :: Qualified (ProperName TypeName)
clsContravariant = Qualified (ByModuleName m_Data_Functor_Contravariant)
  (ProperName "Contravariant")

s_cmap :: String
s_cmap = "cmap"

i_cmap :: Qualified Ident
i_cmap = Qualified (ByModuleName m_Data_Functor_Contravariant) (Ident "cmap")

m_Control_Monad_Eff :: ModuleName
m_Control_Monad_Eff = ModuleName "Control.Monad.Eff"

m_Control_Monad_Eff_Uncurried :: ModuleName
m_Control_Monad_Eff_Uncurried = ModuleName "Control.Monad.Eff.Uncurried"

p_mkEffFn :: Tuple ModuleName String
p_mkEffFn = Tuple m_Control_Monad_Eff_Uncurried "mkEffFn"

p_runEffFn :: Tuple ModuleName String
p_runEffFn = Tuple m_Control_Monad_Eff_Uncurried "runEffFn"

m_Effect :: ModuleName
m_Effect = ModuleName "Effect"

m_Effect_Uncurried :: ModuleName
m_Effect_Uncurried = ModuleName "Effect.Uncurried"

p_mkEffectFn :: Tuple ModuleName String
p_mkEffectFn = Tuple m_Effect_Uncurried "mkEffectFn"

p_runEffectFn :: Tuple ModuleName String
p_runEffectFn = Tuple m_Effect_Uncurried "runEffectFn"

m_Data_Bifoldable :: ModuleName
m_Data_Bifoldable = ModuleName "Data.Bifoldable"

clsBifoldable :: Qualified (ProperName TypeName)
clsBifoldable = Qualified (ByModuleName m_Data_Bifoldable) (ProperName "Bifoldable")

s_bifoldMap :: String
s_bifoldMap = "bifoldMap"

i_bifoldMap :: Qualified Ident
i_bifoldMap = Qualified (ByModuleName m_Data_Bifoldable) (Ident "bifoldMap")

s_bifoldl :: String
s_bifoldl = "bifoldl"

i_bifoldl :: Qualified Ident
i_bifoldl = Qualified (ByModuleName m_Data_Bifoldable) (Ident "bifoldl")

s_bifoldr :: String
s_bifoldr = "bifoldr"

i_bifoldr :: Qualified Ident
i_bifoldr = Qualified (ByModuleName m_Data_Bifoldable) (Ident "bifoldr")

m_Data_Bitraversable :: ModuleName
m_Data_Bitraversable = ModuleName "Data.Bitraversable"

clsBitraversable :: Qualified (ProperName TypeName)
clsBitraversable = Qualified (ByModuleName m_Data_Bitraversable) (ProperName "Bitraversable")

i_bitraverse :: Qualified Ident
i_bitraverse = Qualified (ByModuleName m_Data_Bitraversable) (Ident "bitraverse")

s_bitraverse :: String
s_bitraverse = "bitraverse"

i_ltraverse :: Qualified Ident
i_ltraverse = Qualified (ByModuleName m_Data_Bitraversable) (Ident "ltraverse")

i_rtraverse :: Qualified Ident
i_rtraverse = Qualified (ByModuleName m_Data_Bitraversable) (Ident "rtraverse")

m_Data_Foldable :: ModuleName
m_Data_Foldable = ModuleName "Data.Foldable"

clsFoldable :: Qualified (ProperName TypeName)
clsFoldable = Qualified (ByModuleName m_Data_Foldable) (ProperName "Foldable")

s_foldMap :: String
s_foldMap = "foldMap"

i_foldMap :: Qualified Ident
i_foldMap = Qualified (ByModuleName m_Data_Foldable) (Ident "foldMap")

s_foldl :: String
s_foldl = "foldl"

i_foldl :: Qualified Ident
i_foldl = Qualified (ByModuleName m_Data_Foldable) (Ident "foldl")

s_foldr :: String
s_foldr = "foldr"

i_foldr :: Qualified Ident
i_foldr = Qualified (ByModuleName m_Data_Foldable) (Ident "foldr")

m_Data_Traversable :: ModuleName
m_Data_Traversable = ModuleName "Data.Traversable"

clsTraversable :: Qualified (ProperName TypeName)
clsTraversable = Qualified (ByModuleName m_Data_Traversable) (ProperName "Traversable")

i_traverse :: Qualified Ident
i_traverse = Qualified (ByModuleName m_Data_Traversable) (Ident "traverse")

s_traverse :: String
s_traverse = "traverse"

m_Data_Function_Uncurried :: ModuleName
m_Data_Function_Uncurried = ModuleName "Data.Function.Uncurried"

s_mkFn :: String
s_mkFn = "mkFn"

p_mkFn :: Tuple ModuleName String
p_mkFn = Tuple m_Data_Function_Uncurried "mkFn"

s_runFn :: String
s_runFn = "runFn"

p_runFn :: Tuple ModuleName String
p_runFn = Tuple m_Data_Function_Uncurried "runFn"

m_Data_Int_Bits :: ModuleName
m_Data_Int_Bits = ModuleName "Data.Int.Bits"

p_and :: Tuple ModuleName String
p_and = Tuple m_Data_Int_Bits "and"

p_complement :: Tuple ModuleName String
p_complement = Tuple m_Data_Int_Bits "complement"

p_or :: Tuple ModuleName String
p_or = Tuple m_Data_Int_Bits "or"

p_shl :: Tuple ModuleName String
p_shl = Tuple m_Data_Int_Bits "shl"

p_shr :: Tuple ModuleName String
p_shr = Tuple m_Data_Int_Bits "shr"

p_xor :: Tuple ModuleName String
p_xor = Tuple m_Data_Int_Bits "xor"

p_zshr :: Tuple ModuleName String
p_zshr = Tuple m_Data_Int_Bits "zshr"

m_Data_Newtype :: ModuleName
m_Data_Newtype = ModuleName "Data.Newtype"

clsNewtype :: Qualified (ProperName TypeName)
clsNewtype = Qualified (ByModuleName m_Data_Newtype) (ProperName "Newtype")

m_Partial_Unsafe :: ModuleName
m_Partial_Unsafe = ModuleName "Partial.Unsafe"

p_unsafePartial :: Tuple ModuleName String
p_unsafePartial = Tuple m_Partial_Unsafe "unsafePartial"

i_unsafePartial :: Qualified Ident
i_unsafePartial = Qualified (ByModuleName m_Partial_Unsafe) (Ident "unsafePartial")

m_Data_Profunctor :: ModuleName
m_Data_Profunctor = ModuleName "Data.Profunctor"

clsProfunctor :: Qualified (ProperName TypeName)
clsProfunctor = Qualified (ByModuleName m_Data_Profunctor) (ProperName "Profunctor")

s_dimap :: String
s_dimap = "dimap"

i_dimap :: Qualified Ident
i_dimap = Qualified (ByModuleName m_Data_Profunctor) (Ident "dimap")

i_lcmap :: Qualified Ident
i_lcmap = Qualified (ByModuleName m_Data_Profunctor) (Ident "lcmap")

i_profunctorRmap :: Qualified Ident
i_profunctorRmap = Qualified (ByModuleName m_Data_Profunctor) (Ident "rmap")

m_Control_Monad_ST_Internal :: ModuleName
m_Control_Monad_ST_Internal = ModuleName "Control.Monad.ST.Internal"

p_modify :: Tuple ModuleName String
p_modify = Tuple m_Control_Monad_ST_Internal "modify"

p_new :: Tuple ModuleName String
p_new = Tuple m_Control_Monad_ST_Internal "new"

p_read :: Tuple ModuleName String
p_read = Tuple m_Control_Monad_ST_Internal "read"

p_run :: Tuple ModuleName String
p_run = Tuple m_Control_Monad_ST_Internal "run"

p_write :: Tuple ModuleName String
p_write = Tuple m_Control_Monad_ST_Internal "write"

m_Control_Monad_ST_Uncurried :: ModuleName
m_Control_Monad_ST_Uncurried = ModuleName "Control.Monad.ST.Uncurried"

p_mkSTFn :: Tuple ModuleName String
p_mkSTFn = Tuple m_Control_Monad_ST_Uncurried "mkSTFn"

p_runSTFn :: Tuple ModuleName String
p_runSTFn = Tuple m_Control_Monad_ST_Uncurried "runSTFn"

m_Unsafe_Coerce :: ModuleName
m_Unsafe_Coerce = ModuleName "Unsafe.Coerce"

p_unsafeCoerce :: Tuple ModuleName String
p_unsafeCoerce = Tuple m_Unsafe_Coerce "unsafeCoerce"
