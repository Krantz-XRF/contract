{-# LANGUAGE RecordWildCards #-}
module Language.Contract.Parser (parseTerm) where

import Text.Parsec
import Text.Parsec.Token

import Control.Monad.Reader

import Language.Contract.AST

type Parser = ParsecT String () (Reader [String])

langDef :: Monad m => GenLanguageDef String () m
langDef = LanguageDef
  { commentStart    = "{-"
  , commentEnd      = "-}"
  , commentLine     = "--"
  , nestedComments  = True
  , identStart      = letter
  , identLetter     = alphaNum <|> oneOf "_'"
  , opStart         = opLetter langDef
  , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , reservedOpNames = ["\\", "->", ":", "."]
  , reservedNames   =
    [ "if", "then", "else"
    , "unit", "true", "false"
    , "iszero", "pred", "succ"
    , "Unit", "Nat", "Bool" ]
  , caseSensitive   = True
  }

lexer :: Monad m => GenTokenParser String () m
lexer = makeTokenParser langDef

unit :: Parser Term
unit = Unit <$ reserved lexer "unit"

nat :: Parser Term
nat = Natural . fromIntegral <$> natural lexer

bool :: Parser Term
bool = Boolean True <$ reserved lexer "true"
  <|> Boolean False <$ reserved lexer "false"

type_ :: Parser Type
type_ = parens lexer type_
  <|> TNatural <$ reserved lexer "Nat"
  <|> TBoolean <$ reserved lexer "Bool"
  <|> TUnit <$ reserved lexer "Unit"
  <|> TArrow <$> braces lexer lambda <*> type_ <* reservedOp lexer "->" <*> type_

atom :: Parser Term
atom = do
  x <- identifier lexer
  names <- asks (flip zip [0..])
  case lookup x names of
    Just n -> pure (Atom n)
    Nothing -> fail "unbound variable."

lambda :: Parser Term
lambda = do
  reservedOp lexer "\\"
  x <- identifier lexer
  reservedOp lexer ":"
  t <- type_
  p <- local (x:) (braces lexer term)
  reservedOp lexer "."
  body <- local (x:) term
  pure (Lambda p t body)

if_ :: Parser Term
if_ = If <$> (reserved lexer "if" *> term)
  <*> (reserved lexer "then" *> term)
  <*> (reserved lexer "else" *> term)

assert_ :: Parser Term
assert_ = Assert <$> braces lexer term <*> term

atomTerm :: Parser Term
atomTerm = parens lexer term
  <|> unit <|> nat <|> bool <|> lambda
  <|> atom <|> if_ <|> assert_
  <|> reserved lexer "iszero" *> fmap IsZero term
  <|> reserved lexer "pred" *> fmap Pred term
  <|> reserved lexer "succ" *> fmap Succ term

term :: Parser Term
term = foldl1 App <$> many1 atomTerm

parseTerm :: String -> Either ParseError Term
parseTerm s =
  let prog = whiteSpace lexer *> term
  in runReader (runParserT prog () "<interactive>" s) []
