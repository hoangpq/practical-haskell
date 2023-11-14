module Parser where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

import People

import Control.Monad
import Control.Monad.Combinators

type Parser = Parsec
  -- The type for custom error messages. We have none, so use `Void`.
  Void
  -- The input stream type. Let's use `String` for now, but for
  -- better performance, you might want to use `Text` or `ByteString`.
  String

word :: Parser String
word = some letterChar

nonNegativeNumber :: Parser Int
nonNegativeNumber = read <$> some digitChar

spaces :: Parser ()
spaces = space

parsePerson :: Parser Person
parsePerson = Person <$> (word <* spaces) <*> (nonNegativeNumber <* spaces)

word' :: Parser String
word' = do
  n <- decimal
  guard (n > 0)
  elts <- count n (spaces *> some letterChar)
  pure (unwords elts)



