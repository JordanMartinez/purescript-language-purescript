------------------------------------
-- This module is code generated. --
--          DO NOT EDIT!          --
------------------------------------
module Language.PureScript.Constants.Prim where

import Language.PureScript.Names (ClassName, TypeName, Ident(..), ModuleName(..), ProperName(..), Qualified(..), QualifiedBy(..))

m_Prim :: ModuleName
m_Prim = ModuleName "Prim"

clsPartial :: Qualified (ProperName ClassName)
clsPartial = Qualified (ByModuleName m_Prim) (ProperName "Partial")

tyArray :: Qualified (ProperName TypeName)
tyArray = Qualified (ByModuleName m_Prim) (ProperName "Array")

tyBoolean :: Qualified (ProperName TypeName)
tyBoolean = Qualified (ByModuleName m_Prim) (ProperName "Boolean")

tyChar :: Qualified (ProperName TypeName)
tyChar = Qualified (ByModuleName m_Prim) (ProperName "Char")

tyConstraint :: Qualified (ProperName TypeName)
tyConstraint = Qualified (ByModuleName m_Prim) (ProperName "Constraint")

tyFunction :: Qualified (ProperName TypeName)
tyFunction = Qualified (ByModuleName m_Prim) (ProperName "Function")

tyInt :: Qualified (ProperName TypeName)
tyInt = Qualified (ByModuleName m_Prim) (ProperName "Int")

tyNumber :: Qualified (ProperName TypeName)
tyNumber = Qualified (ByModuleName m_Prim) (ProperName "Number")

tyRecord :: Qualified (ProperName TypeName)
tyRecord = Qualified (ByModuleName m_Prim) (ProperName "Record")

tyRow :: Qualified (ProperName TypeName)
tyRow = Qualified (ByModuleName m_Prim) (ProperName "Row")

tyString :: Qualified (ProperName TypeName)
tyString = Qualified (ByModuleName m_Prim) (ProperName "String")

tySymbol :: Qualified (ProperName TypeName)
tySymbol = Qualified (ByModuleName m_Prim) (ProperName "Symbol")

tyType :: Qualified (ProperName TypeName)
tyType = Qualified (ByModuleName m_Prim) (ProperName "Type")

s_undefined :: String
s_undefined = "undefined"

i_undefined :: Qualified Ident
i_undefined = Qualified (ByModuleName m_Prim) (Ident "undefined")

m_Prim_Boolean :: ModuleName
m_Prim_Boolean = ModuleName "Prim.Boolean"

tyFalse :: Qualified (ProperName TypeName)
tyFalse = Qualified (ByModuleName m_Prim_Boolean) (ProperName "False")

tyTrue :: Qualified (ProperName TypeName)
tyTrue = Qualified (ByModuleName m_Prim_Boolean) (ProperName "True")

m_Prim_Coerce :: ModuleName
m_Prim_Coerce = ModuleName "Prim.Coerce"

clsCoercible :: Qualified (ProperName ClassName)
clsCoercible = Qualified (ByModuleName m_Prim_Coerce) (ProperName "Coercible")

m_Prim_Int :: ModuleName
m_Prim_Int = ModuleName "Prim.Int"

clsIntAdd :: Qualified (ProperName ClassName)
clsIntAdd = Qualified (ByModuleName m_Prim_Int) (ProperName "Add")

clsIntCompare :: Qualified (ProperName ClassName)
clsIntCompare = Qualified (ByModuleName m_Prim_Int) (ProperName "Compare")

clsIntMul :: Qualified (ProperName ClassName)
clsIntMul = Qualified (ByModuleName m_Prim_Int) (ProperName "Mul")

clsIntToString :: Qualified (ProperName ClassName)
clsIntToString = Qualified (ByModuleName m_Prim_Int) (ProperName "ToString")

m_Prim_Ordering :: ModuleName
m_Prim_Ordering = ModuleName "Prim.Ordering"

tyTypeOrdering :: Qualified (ProperName TypeName)
tyTypeOrdering = Qualified (ByModuleName m_Prim_Ordering) (ProperName "Ordering")

tyEQ :: Qualified (ProperName TypeName)
tyEQ = Qualified (ByModuleName m_Prim_Ordering) (ProperName "EQ")

tyGT :: Qualified (ProperName TypeName)
tyGT = Qualified (ByModuleName m_Prim_Ordering) (ProperName "GT")

tyLT :: Qualified (ProperName TypeName)
tyLT = Qualified (ByModuleName m_Prim_Ordering) (ProperName "LT")

m_Prim_Row :: ModuleName
m_Prim_Row = ModuleName "Prim.Row"

clsRowCons :: Qualified (ProperName ClassName)
clsRowCons = Qualified (ByModuleName m_Prim_Row) (ProperName "Cons")

clsRowLacks :: Qualified (ProperName ClassName)
clsRowLacks = Qualified (ByModuleName m_Prim_Row) (ProperName "Lacks")

clsRowNub :: Qualified (ProperName ClassName)
clsRowNub = Qualified (ByModuleName m_Prim_Row) (ProperName "Nub")

clsRowUnion :: Qualified (ProperName ClassName)
clsRowUnion = Qualified (ByModuleName m_Prim_Row) (ProperName "Union")

m_Prim_RowList :: ModuleName
m_Prim_RowList = ModuleName "Prim.RowList"

tyRowList :: Qualified (ProperName TypeName)
tyRowList = Qualified (ByModuleName m_Prim_RowList) (ProperName "RowList")

clsRowToList :: Qualified (ProperName ClassName)
clsRowToList = Qualified (ByModuleName m_Prim_RowList) (ProperName "RowToList")

tyRowListCons :: Qualified (ProperName TypeName)
tyRowListCons = Qualified (ByModuleName m_Prim_RowList) (ProperName "Cons")

tyRowListNil :: Qualified (ProperName TypeName)
tyRowListNil = Qualified (ByModuleName m_Prim_RowList) (ProperName "Nil")

m_Prim_Symbol :: ModuleName
m_Prim_Symbol = ModuleName "Prim.Symbol"

clsSymbolAppend :: Qualified (ProperName ClassName)
clsSymbolAppend = Qualified (ByModuleName m_Prim_Symbol) (ProperName "Append")

clsSymbolCompare :: Qualified (ProperName ClassName)
clsSymbolCompare = Qualified (ByModuleName m_Prim_Symbol) (ProperName "Compare")

clsSymbolCons :: Qualified (ProperName ClassName)
clsSymbolCons = Qualified (ByModuleName m_Prim_Symbol) (ProperName "Cons")

m_Prim_TypeError :: ModuleName
m_Prim_TypeError = ModuleName "Prim.TypeError"

clsFail :: Qualified (ProperName ClassName)
clsFail = Qualified (ByModuleName m_Prim_TypeError) (ProperName "Fail")

clsWarn :: Qualified (ProperName ClassName)
clsWarn = Qualified (ByModuleName m_Prim_TypeError) (ProperName "Warn")

tyAbove :: Qualified (ProperName TypeName)
tyAbove = Qualified (ByModuleName m_Prim_TypeError) (ProperName "Above")

tyBeside :: Qualified (ProperName TypeName)
tyBeside = Qualified (ByModuleName m_Prim_TypeError) (ProperName "Beside")

tyDoc :: Qualified (ProperName TypeName)
tyDoc = Qualified (ByModuleName m_Prim_TypeError) (ProperName "Doc")

tyQuote :: Qualified (ProperName TypeName)
tyQuote = Qualified (ByModuleName m_Prim_TypeError) (ProperName "Quote")

tyQuoteLabel :: Qualified (ProperName TypeName)
tyQuoteLabel = Qualified (ByModuleName m_Prim_TypeError) (ProperName "QuoteLabel")

tyText :: Qualified (ProperName TypeName)
tyText = Qualified (ByModuleName m_Prim_TypeError) (ProperName "Text")
