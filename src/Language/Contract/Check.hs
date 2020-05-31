module Language.Contract.Check where

import Data.List
import Data.Bifunctor
import Text.Printf

import Control.Monad.Trans.Maybe
import Control.Monad.Reader

import Language.Contract.AST

type MonadTypeCheck m =
  ( MonadReader ([Type], [Term]) m
  , MonadFail m
  , MonadIO m )

runTypeCheck :: MaybeT (ReaderT ([Type], [Term]) IO) a -> IO (Maybe a)
runTypeCheck m = runReaderT (runMaybeT m) ([], [])

assert :: MonadFail m => Bool -> m ()
assert c = unless c (fail "")

prove :: MonadTypeCheck m => Term -> m ()
prove res = do
  n <- asks (length . fst)
  premise <- asks snd
  liftIO $ printf "%s => %s [%s]\n"
    (pretty n premise) (pretty n $ eval res) (pretty n res)

-- |The type of a term, within a specific context.
typeOf :: MonadTypeCheck m => Term -> m Type
typeOf Unit = pure TUnit
typeOf (Lambda p t m) = do
  TBoolean <- local (first (t:)) (typeOf p)
  TArrow p t <$> local (bimap (t:) (p:)) (typeOf m)
typeOf (App f x) = do
  TArrow p tx tr <- typeOf f
  tx' <- typeOf x
  assert (tx == tx')
  prove (Lambda (Boolean True) tx p `App` x)
  pure tr
typeOf (Assert p x) = do
  t <- typeOf x
  local (first (t:)) $ do
    TBoolean <- typeOf p
    prove p
  pure t
typeOf (Atom n) = do
  ts <- asks fst
  assert (genericLength ts > n)
  pure (ts `genericIndex` n)
typeOf (If c t f) = do
  TBoolean <- typeOf c
  tt <- local (second (c:)) (typeOf t)
  tf <- local (second (Not c:)) (typeOf f)
  assert (tt == tf)
  pure tt
typeOf (Succ n) = do
  TNatural <- typeOf n
  pure TNatural
typeOf (Pred n) = do
  TNatural <- typeOf n
  prove (Not $ IsZero n)
  pure TNatural
typeOf (IsZero t) = do
  TNatural <- typeOf t
  pure TBoolean
typeOf (Boolean _) = pure TBoolean
typeOf (Natural _) = pure TNatural

-- |One-step evaluation of a term.
eval1 :: [Term] -> Term -> Term
eval1 vs (App (Lambda _ _ m) x@Value) = eval1 (x:vs) m
eval1 vs (App f@(Lambda _ _ _) x) = App f (eval1 vs x)
eval1 vs (App f x) = App (eval1 vs f) x
eval1 vs (Atom n) =
  if genericLength vs > n
  then vs `genericIndex` n
  else Atom n
eval1 _  (If (Boolean c) t f) = if c then t else f
eval1 vs (If c t f) = If (eval1 vs c) t f
eval1 _  (Succ (Natural n)) = Natural (n + 1)
eval1 vs (Succ n) = Succ (eval1 vs n)
eval1 _  (Pred (Natural n)) = Natural (n - 1)
eval1 vs (Pred n) = Pred (eval1 vs n)
eval1 _  (IsZero (Natural n)) = Boolean (n == 0)
eval1 vs (IsZero n) = IsZero (eval1 vs n)
eval1 _  v = v

-- |Full evaluation of a term.
eval :: Term -> Term
eval t = let x = eval1 [] t in
  if isValue x || t == x then x else eval x
