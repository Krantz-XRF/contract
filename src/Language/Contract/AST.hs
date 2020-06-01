module Language.Contract.AST where

import Numeric.Natural

-- |Types: Nat, Bool, * -> *
data Type
  = TUnit
  | TNatural
  | TBoolean
  | TArrow Term Type Type
  deriving stock (Show, Eq)

-- |Terms of STLC.
data Term
  = Unit
  | Lambda Term Type Term
  | App Term Term
  | Assert Term Term
  | Atom Natural
  | If Term Term Term
  | Succ Term
  | Pred Term
  | IsZero Term
  | Natural Natural
  | Boolean Bool
  deriving stock (Show, Eq)

pattern And, Or :: Term -> Term -> Term
pattern And x y = If x y (Boolean False)
pattern Or x y = If x (Boolean True) y

pattern Not :: Term -> Term
pattern Not x = If x (Boolean False) (Boolean True)

-- |Is this term a value?
isValue :: Term -> Bool
isValue Unit = True
isValue (Lambda _ _ _) = True
isValue (Natural _) = True 
isValue (Boolean _) = True
isValue _ = False

-- |Bind to a value.
pattern Value :: Term
pattern Value <- (isValue -> True)
