module Language.Contract.Proof where

import Control.Monad.Reader

import Language.Contract.AST

-- type MonadCheck m =
--   ( MonadSymbolic m
--   , MonadIO m
--   , MonadReader [(Term, SBool)] m )

-- tryProve :: MonadIO m => [Type] -> [Term] -> Term -> m ThmResult
-- tryProve bindings premises target = liftIO $ prove $ do
--   vars <- forM (zip [0..] bindings) $ \(n, t) ->
--     free (makeVarName n) :: Symbolic SBool
--   pure sTrue

naiveTryProve :: (MonadFail m, MonadIO m) => [Type] -> [Term] -> Term -> m ()
naiveTryProve _ _ (Boolean True) = liftIO (putStrLn "Q.E.D. (Trivial)")
naiveTryProve _ [] _ = liftIO (putStrLn "Falsified") >> fail "Falsified"
naiveTryProve _ premise _ | Boolean False `elem` premise
  = liftIO (putStrLn "Q.E.D. (Vacuous)")
naiveTryProve _ premise t | t `elem` premise = liftIO (putStrLn "Q.E.D.")
naiveTryProve _ _ _ = liftIO (putStrLn "Unknown")
