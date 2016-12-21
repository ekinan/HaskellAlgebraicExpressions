module Parser
(
  Parser(..),
) where

import Control.Applicative
import Data.Char

{-
This is the source file containing the parser. The parser is identical to the one
used in HWs 9 and 10 of the CIS 194 course, but I used the one in HW 9 since those
were based on my solutions (some functions were provided by the course, and these
are noted below).

I understand that there's a Parsec library out there that has many of these functions.
However, I wanted to see what it would be like to write the parser for the language
from scratch myself so that I could get a good experience using Monads and Functors.
-}

-- A parser for a value of type a is a function which takes a String
-- representing the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

-- Functor instances of the Parser, taken from my solutions for HW 9
-- of CIS 194
instance Functor Parser where
  fmap f (Parser g) = Parser (\s -> case (g s) of
                                      (Just res) -> Just (first f res)
                                      Nothing    -> Nothing)
                                      
instance Applicative Parser where
  pure x = Parser (\s -> Just (x, s))
  (Parser f) <*> (Parser g) 
    = Parser (\s -> case (f s) of
                          (Just (h, res)) -> fmap (first h) $ g res
                          Nothing         -> Nothing)
                         
instance Alternative Parser where
  empty = Parser (\s -> Nothing)
  (<|>) (Parser f1) (Parser f2) = Parser (\s -> f1 s <|> f2 s)
  
-- Monad instance for the parser
{-
Note that the semantics for >>= are as follows
  1. Run the given parser p
  2. If it yields something, then create a new parser using its result and run it
     on the remaining input.
   
     Otherwise, indicate failure and return nothing.
-}
instance Monad Parser where
  return x = pure x
  (Parser p) >>= f  
    = Parser (\s -> case (p s) of
                         (Just (r, res)) -> runParser (f r) res
                         Nothing         -> Nothing)
