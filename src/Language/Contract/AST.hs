module Language.Contract.AST where

import Data.List
import Numeric.Natural
import Control.Monad
import Language.Contract.Type

-- |Terms of STLC.
data Term
  = Lambda Type Term
  | App Term Term
  | Atom Natural
  | If Term Term Term
  | Succ Term
  | Pred Term
  | Natural Natural
  | Boolean Bool
  deriving stock (Show, Eq)

-- |Is this term a value?
isValue :: Term -> Bool
isValue (Lambda _ _) = True
isValue (Natural _) = True 
isValue (Boolean _) = True
isValue _ = False

-- |Bind to a value.
pattern Value :: Term
pattern Value <- (isValue -> True)

-- |The type of a term, within a specific context.
typeOf :: [Type] -> Term -> Maybe Type
typeOf ts (Lambda t m) = TArrow t <$> typeOf (t:ts) m
typeOf ts (App f x) = do
  TArrow tx tr <- typeOf ts f
  tx' <- typeOf ts x
  guard (tx == tx')
  pure tr
typeOf ts (Atom n) = do
  guard (genericLength ts > n)
  pure (ts `genericIndex` n)
typeOf ts (If c t f) = do
  TBoolean <- typeOf ts c
  tt <- typeOf ts t
  tf <- typeOf ts f
  guard (tt == tf)
  pure tt
typeOf ts (Succ n) = do
  TNatural <- typeOf ts n
  pure TNatural
typeOf ts (Pred n) = do
  TNatural <- typeOf ts n
  pure TNatural
typeOf _ (Boolean _) = pure TBoolean
typeOf _ (Natural _) = pure TNatural

-- |One-step evaluation of a term.
eval1 :: [Term] -> Term -> Term
eval1 vs (App (Lambda _ m) x@Value) = eval1 (x:vs) m
eval1 vs (App f@(Lambda _ _) x) = App f (eval1 vs x)
eval1 vs (App f x) = App (eval1 vs f) x
eval1 vs (Atom n) = vs `genericIndex` n
eval1 _  (If (Boolean c) t f) = if c then t else f
eval1 vs (If c t f) = If (eval1 vs c) t f
eval1 _  (Succ (Natural n)) = Natural (n + 1)
eval1 vs (Succ n) = Succ (eval1 vs n)
eval1 _  (Pred (Natural n)) = Natural (n - 1)
eval1 vs (Pred n) = Pred (eval1 vs n)
eval1 _  v = v

-- |Full evaluation of a term.
eval :: Term -> Term
eval t = let x = eval1 [] t in
  if isValue x || t == x then x else eval x
