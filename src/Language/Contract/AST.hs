{-|
Module      : Language.Contract.AST
Description : Abstract Syntax Tree for the contract language.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
module Language.Contract.AST where

import Numeric.Natural

-- |Types: Nat, Bool, * -> *
data Type
  = TUnit
  | TNatural
  | TBoolean
  | TArrow Term Type Type
  deriving stock (Show, Eq, Ord)

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
  deriving stock (Show, Eq, Ord)

-- |@x & y = if x then y else false@
pattern And :: Term -> Term -> Term
pattern And x y = If x y (Boolean False)

-- |@x | y = if x then true else y@
pattern Or :: Term -> Term -> Term
pattern Or x y = If x (Boolean True) y

-- |@not x = if x then false else true@
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

-- |The lift operation of de Bruijn notation.
liftAtom :: Natural -> Natural -> Term -> Term
liftAtom c n (Lambda p t m)
  = Lambda (liftAtom (succ c) n p) t (liftAtom (succ c) n m)
liftAtom c n (App f x) = App (liftAtom c n f) (liftAtom c n x)
liftAtom c n (Assert p x) = Assert (liftAtom c n p) (liftAtom c n x)
liftAtom c n (Atom k) = if k >= c then Atom (k + n) else Atom k
liftAtom c n (If b t f) = If (liftAtom c n b) (liftAtom c n t) (liftAtom c n f)
liftAtom c n (Succ t) = Succ (liftAtom c n t)
liftAtom c n (Pred t) = Pred (liftAtom c n t)
liftAtom c n (IsZero t) = IsZero (liftAtom c n t)
liftAtom _ _ x = x
