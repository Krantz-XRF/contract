module Language.Contract.Type where

-- |Types: Nat, Bool, * -> *
data Type
  = TUnit
  | TNatural
  | TBoolean
  | TArrow Type Type
  deriving stock (Show, Eq)
