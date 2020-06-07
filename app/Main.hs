{-|
Module      : Main
Description : Main program for contract interpreter.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
module Main where

import System.IO
import System.Exit
import System.Console.GetOpt
import System.Environment

import Data.Bifunctor
import Text.Printf
import Control.Monad

import Language.Contract.AST
import Language.Contract.Check
import Language.Contract.Parser
import Language.Contract.Pretty

data Flag = PrintHelp | PrintVersion | Interactive
  deriving stock (Eq, Show, Ord)

options :: [OptDescr Flag]
options =
  [ Option ['V'] ["version"]     (NoArg PrintVersion) "print version number"
  , Option ['?'] ["help"]        (NoArg PrintHelp)    "print help message"
  , Option ['I'] ["interactive"] (NoArg Interactive)  "open REPL"
  ]

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    (_, _, errs@(_:_)) -> mapM_ putStrLn errs
    (opts, files, []) -> do
      when (PrintVersion `elem` opts) $ putStrLn "contract 2020"
      when (PrintHelp `elem` opts) $ putStrLn $
        usageInfo "contract [OPTIONS...] FILES..." options
      mapM_ processFile files
      when (Interactive `elem` opts) repl

repl :: IO ()
repl = forever $ do
  putStr "contract> "
  hFlush stdout
  input <- getLine
  when (input == "quit") exitSuccess
  case parseTerm input of
    Left err -> print err
    Right tm -> processTerm Nothing tm

processFile :: FilePath -> IO ()
processFile file = withFile file ReadMode $ \h -> do
  contents <- hGetContents h
  case parseFile file contents of
    Left err -> print err
    Right ts -> mapM_ (uncurry processTerm) (map (first Just) ts)

processTerm :: Maybe String -> Term -> IO ()
processTerm nm tm = runTypeCheck (typeOf tm) >>= \case
  Nothing -> do
    printf "error: the program "
    case nm of
      Nothing -> printf "'%s'" (pretty tm)
      Just n -> printf "'%s' (%s)" n (pretty tm)
    printf " does not type check.\n"
  Just t -> do
    case nm of
      Nothing -> pure ()
      Just n -> printf "%s = " n
    putStr $ pretty (eval tm)
    putStr " : "
    prettyPrint t
