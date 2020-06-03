module Language.Contract.Proof where

import Data.List
import Data.SBV.Trans
import Numeric.Natural

import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import Language.Contract.AST
import Language.Contract.Pretty

type MonadCheck m =
  ( MonadSymbolic m
  , MonadIO m
  , MonadReader [Var] m )

data Var
  = VBool SBool
  | VNat SInteger
  | VUnit
  | VFunc (Var -> Var)

asNat :: Var -> SInteger
asNat ~(VNat n) = n

asBool :: Var -> SBool
asBool ~(VBool b) = b

asFunc :: Var -> Var -> Var
asFunc ~(VFunc f) = f

freeVar :: (MonadFail m, MonadSymbolic m) => Natural -> Type -> m Var
freeVar n TBoolean = VBool <$> free (makeVarName n)
freeVar n TNatural = VNat <$> free (makeVarName n)
freeVar _ TUnit = pure VUnit
freeVar _ _ = fail "Function type not supported in SBV."

iteVar :: Var -> Var -> Var -> Var
iteVar ~(VBool b) t f = case (t, f) of
  (VNat x, ~(VNat y)) -> VNat (ite b x y)
  (VBool x, ~(VBool y)) -> VBool (ite b x y)
  (_, _) -> VUnit

makeBoolTerm :: [Var] -> Term -> SBool
makeBoolTerm vs = asBool . makeTerm vs

constrainPremise :: MonadIO m => [Var] -> Term -> SymbolicT m ()
constrainPremise vs = constrain . makeBoolTerm vs

makeTerm :: [Var] -> Term -> Var
makeTerm _  Unit = VUnit
makeTerm vs (Lambda _ _ t) = VFunc $ \x -> makeTerm (x:vs) t
makeTerm vs (App f x) = asFunc (makeTerm vs f) (makeTerm vs x)
makeTerm vs (Assert _ t) = makeTerm vs t
makeTerm vs (Atom n) = vs `genericIndex` n
makeTerm vs (If b t f) = iteVar (makeTerm vs b) (makeTerm vs t) (makeTerm vs f)
makeTerm vs (Succ n) = VNat . (+ 1) . asNat $ makeTerm vs n
makeTerm vs (Pred n) = VNat . subtract 1 . asNat $ makeTerm vs n
makeTerm vs (IsZero n) = VBool . (.== 0) . asNat $ makeTerm vs n
makeTerm _  (Natural n) = VNat $ literal $ fromIntegral n
makeTerm _  (Boolean b) = VBool $ literal b

pattern Falsified :: ThmResult
pattern Falsified <- ThmResult (isFalsified -> True)

isFalsified :: SMTResult -> Bool
isFalsified (Satisfiable _ _) = True
isFalsified (SatExtField _ _) = True
isFalsified _ = False

pattern Proven :: ThmResult
pattern Proven <- ThmResult (Unsatisfiable _ _)

tryProve :: MonadIO m => [Type] -> [Term] -> Term -> m (Maybe ThmResult)
tryProve bindings premises target =
  let prove' = prove :: SymbolicT (MaybeT IO) SBool -> MaybeT IO ThmResult
  in liftIO $ runMaybeT $ prove' $ do
  vars <- forM (zip [0..] bindings) (uncurry freeVar)
  forM_ vars $ \case
    VNat x -> constrain (x .>= 0)
    _ -> pure ()
  mapM_ (constrainPremise vars) premises
  pure (makeBoolTerm vars target)

naiveTryProve :: (MonadFail m, MonadIO m) => [Type] -> [Term] -> Term -> m ()
naiveTryProve _ _ (Boolean True) = liftIO (putStrLn "Q.E.D. (Trivial)")
naiveTryProve _ [] _ = liftIO (putStrLn "Falsified") >> fail "Falsified"
naiveTryProve _ premise _ | Boolean False `elem` premise
  = liftIO (putStrLn "Q.E.D. (Vacuous)")
naiveTryProve _ premise t | t `elem` premise = liftIO (putStrLn "Q.E.D.")
naiveTryProve _ _ _ = liftIO (putStrLn "Unknown")
