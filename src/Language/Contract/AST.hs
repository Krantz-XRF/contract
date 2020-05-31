module Language.Contract.AST where

import Data.List
import Data.Bifunctor
import Numeric.Natural

import Control.Monad.Reader
import Control.Monad.Writer

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

type MonadPretty m = (MonadReader (Int, [String]) m, MonadWriter String m)

newVar :: MonadPretty m => (String -> m a) -> m a
newVar p = do
  x <- asks (head . snd)
  let x' = map succ x
  local (second (x':)) (p x')

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
prettyType (TArrow p s t) = do
  tell "{"
  prettyTerm p
  tell "} "
  prettyType s
  tell " -> "
  prettyType t

prettyTerm :: MonadPretty m => Term -> m ()
prettyTerm (Not t) = tellParen 3 $ tell "not " *> paren 4 (prettyTerm t)
prettyTerm (And x y) = tellParen 2 $ do
  prettyTerm x
  tell " and "
  prettyTerm y
prettyTerm (Or x y) = tellParen 1 $ do
  prettyTerm x
  tell " or "
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
  x <- asks ((`genericIndex` n) . snd)
  tell x
prettyTerm (If c t f) = tellParen 0 $ do
  tell "if "
  prettyTerm c
  tell " then "
  prettyTerm t
  tell " else "
  prettyTerm f
prettyTerm (Succ t) = tellParen 3 $ tell "succ " *> paren 4 (prettyTerm t)
prettyTerm (Pred t) = tellParen 3 $ tell "pred " *> paren 4 (prettyTerm t)
prettyTerm (IsZero t) = tellParen 3 $ tell "iszero " *> paren 4 (prettyTerm t)
prettyTerm (Natural n) = tell (show n)
prettyTerm (Boolean b) = tell (if b then "true" else "false")

class PrettyPrint a where
  pretty :: Int -> a -> String

prettyPrint :: PrettyPrint a => Int -> a -> IO ()
prettyPrint u = putStrLn . pretty u

instance PrettyPrint Type where
  pretty m t = runReader (execWriterT (prettyType t)) (0, vars m ["`"])
    where vars 0 xs = xs
          vars n xs = vars (pred n) (map succ (head xs) : xs)

instance PrettyPrint Term where
  pretty m t = runReader (execWriterT (prettyTerm t)) (0, vars m ["`"])
    where vars 0 xs = xs
          vars n xs = vars (pred n) (map succ (head xs) : xs)

instance PrettyPrint a => PrettyPrint [a] where
  pretty n ys = "[" <> go ys <> "]" where
    go [] = ""
    go [x] = pretty n x
    go (x : xs) = pretty n x <> ", " <> go xs
