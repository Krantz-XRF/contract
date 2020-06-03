module Language.Contract.Pretty where

import Data.Char
import Data.Bifunctor
import Numeric.Natural

import Control.Monad.Reader
import Control.Monad.Writer

import Language.Contract.AST

type MonadPretty m = (MonadReader (Int, Natural) m, MonadWriter String m)

newVar :: MonadPretty m => (String -> m a) -> m a
newVar p = do
  x <- asks (makeVarName . snd)
  local (second (+1)) (p x)

makeVarName :: Natural -> String
makeVarName = toStr . go [] where
  go xs 0 = xs
  go xs n = let (q, r) = quotRem n 26 in go (r : xs) q
  toStr [] = "a"
  toStr xs = map (chr . (+ ord 'a') . fromIntegral) xs

paren :: MonadPretty m => Int -> m a -> m a
paren n = local (first (const n))

tellParen :: MonadPretty m => Int -> m a -> m a
tellParen n m = do
  p <- asks fst
  when (n < p) (tell "(")
  res <- paren n m
  when (n < p) (tell ")")
  pure res

prettyType :: MonadPretty m => Type -> m ()
prettyType TUnit = tell "Unit"
prettyType TNatural = tell "Nat"
prettyType TBoolean = tell "Bool"
prettyType (TArrow p s t) = local (second (+1)) $ do
  tell "{"
  prettyTerm p
  tell "} "
  prettyType s
  tell " -> "
  prettyType t

prettyTerm :: MonadPretty m => Term -> m ()
prettyTerm (Not t) = tellParen 5 $ tell "not " *> paren 6 (prettyTerm t)
prettyTerm (And x y) = tellParen 2 $ do
  prettyTerm x
  tell " & "
  prettyTerm y
prettyTerm (Or x y) = tellParen 1 $ do
  prettyTerm x
  tell " | "
  prettyTerm y
prettyTerm Unit = tell "unit"
prettyTerm (Lambda p t tm) = newVar $ \x -> tellParen 0 $ do
  tell "\\"
  tell x
  tell " : "
  prettyType t
  tell " {"
  prettyTerm p
  tell "} . "
  prettyTerm tm
prettyTerm (App f x) = tellParen 5 $ do
  prettyTerm f
  tell " "
  paren 6 $ prettyTerm x
prettyTerm (Assert p x) = tellParen 3 $ do
  tell "{"
  prettyTerm p
  tell "} "
  paren 4 $ prettyTerm x
prettyTerm (Atom n) = do
  x <- asks snd
  tell (makeVarName $ x - n - 1)
prettyTerm (If c t f) = tellParen 0 $ do
  tell "if "
  prettyTerm c
  tell " then "
  prettyTerm t
  tell " else "
  prettyTerm f
prettyTerm (Succ t) = tellParen 5 $ tell "succ " *> paren 6 (prettyTerm t)
prettyTerm (Pred t) = tellParen 5 $ tell "pred " *> paren 6 (prettyTerm t)
prettyTerm (IsZero t) = tellParen 5 $ tell "iszero " *> paren 6 (prettyTerm t)
prettyTerm (Natural n) = tell (show n)
prettyTerm (Boolean b) = tell (if b then "true" else "false")

class PrettyPrint a where
  prettyWith :: Natural -> a -> String
  pretty :: a -> String

  prettyWith _ = pretty
  pretty = prettyWith 0
  {-# MINIMAL prettyWith | pretty #-}

prettyPrintWith :: PrettyPrint a => Natural -> a -> IO ()
prettyPrintWith u = putStrLn . prettyWith u

prettyPrint :: PrettyPrint a => a -> IO ()
prettyPrint = putStrLn . pretty

instance PrettyPrint Type where
  prettyWith m t = runReader (execWriterT (prettyType t)) (0, m)

instance PrettyPrint Term where
  prettyWith m t = runReader (execWriterT (prettyTerm t)) (0, m)

instance PrettyPrint a => PrettyPrint [a] where
  prettyWith n ys = "[" <> go ys <> "]" where
    go [] = ""
    go [x] = prettyWith n x
    go (x : xs) = prettyWith n x <> ", " <> go xs
