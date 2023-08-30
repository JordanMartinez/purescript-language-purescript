module GenConstants.Prim where

import Prelude hiding (mod)

import GenConstants.TH (AppM, asIdent, asString, cls, clss, mod, prefixWith, ty, tys, var)

codegenPrimModule :: Partial => AppM Unit
codegenPrimModule = do
  mod "Prim" do
    cls "Partial"
    ty "Array"
    ty "Boolean"
    ty "Char"
    ty "Constraint"
    ty "Function"
    ty "Int"
    ty "Number"
    ty "Record"
    ty "Row"
    ty "String"
    ty "Symbol"
    ty "Type"
    asIdent do asString do var "undefined"

  mod "Prim.Boolean" do
    tys [ "False", "True" ]

  mod "Prim.Coerce" do
    cls "Coercible"

  mod "Prim.Int" do
    prefixWith "Int" do clss [ "Add", "Compare", "Mul", "ToString" ]

  mod "Prim.Ordering" do
    prefixWith "Type" do ty "Ordering"
    tys [ "EQ", "GT", "LT" ]

  mod "Prim.Row" do
    prefixWith "Row" do clss [ "Cons", "Lacks", "Nub", "Union" ]

  mod "Prim.RowList" do
    ty "RowList"
    cls "RowToList"
    prefixWith "RowList" do tys [ "Cons", "Nil" ]

  mod "Prim.Symbol" do
    prefixWith "Symbol" do clss [ "Append", "Compare", "Cons" ]

  mod "Prim.TypeError" do
    clss [ "Fail", "Warn" ]
    tys [ "Above", "Beside", "Doc", "Quote", "QuoteLabel", "Text" ]
