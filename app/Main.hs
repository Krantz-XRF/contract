module Main where

import System.IO
import System.Exit

import Control.Monad

import Language.Contract.Check
import Language.Contract.Parser
import Language.Contract.Pretty

main :: IO ()
main = forever $ do
  putStr "contract> "
  hFlush stdout
  input <- getLine
  when (input == "quit") exitSuccess
  case parseTerm input of
    Left err -> print err
    Right tm -> runTypeCheck (typeOf tm) >>= \case
      Nothing -> putStrLn "error: the program does not type check."
      Just t -> do
        putStr $ pretty (eval tm)
        putStr " : "
        prettyPrint t
