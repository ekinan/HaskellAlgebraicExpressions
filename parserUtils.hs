module ParserUtils where
import Parser

import Control.Applicative hiding (optional)
import Data.Char
import Prelude hiding(filter)

{-
This contains helpful, generic utility functions that can be used for
different parsers in the language (e.g. an expression parser, a program
parser).
-}

-- GENERIC PARSER METHODS

-- Returns a new parser that tries to run the given parser.
-- If it succeeds, then the results of that parser are returned,
-- otherwise a dummy value is returned with the input unprocessed.
optional :: a -> Parser a -> Parser a
optional x p = p <|> pure x

-- Identical to the "many" function in Parsec
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- Identical to the "many1" function in Parsec
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

-- Takes in a list of parsers and creates a new parser
-- that applies each parser in sequence until one of them
-- succeeds. Note the list must be non-empty.
alt :: [Parser a] -> Parser a
alt ps@(_:_)  = foldl1 (<|>) ps
alt _         = undefined
                 
-- CHAR + STRING PARSER METHODS

-- From the CIS 194 homework handout.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail


-- From the CIS 194 homework hand out
char :: Char -> Parser Char
char c = satisfy (== c)

-- Returns a parser that will optionally look
-- for the desired character, returning the empty
-- string if the character is not found.
optionalChar :: Char -> Parser String
optionalChar c = optional "" ((:[]) <$> char c)

-- Creates a parser that will parse out the given string
string :: String -> Parser String
string s = foldr addToParser (pure []) s
 where
  addToParser :: Char -> Parser String -> Parser String
  addToParser ch p = do c  <- char ch
                        cs <- p
                        return (c:cs)
                        
-- Creates a parser that will filter out the given character
-- from the input.
filterChar :: Char -> Parser String
filterChar ch 
  = (zeroOrMore $ filterOne) >>= (\res -> (return $ concat $ res))
 where 
  filterOne = do zeroOrMore $ char ch
                 res <- oneOrMore $ satisfy (\c -> c /= ch)
                 zeroOrMore $ char ch
                 return res

-- PARSERS FOR NUMBERS

-- Returns a parser that parses out numeric digits
-- from the input, note there can be leading zeroes
digitsParser :: Parser String
digitsParser = oneOrMore (satisfy isDigit)

-- Parses a positive integer
posInt :: Parser Int
posInt = read <$> (zeroOrMore (char '0') *> digitsParser)

-- Parses numbers of the form '.'D+ where D is a numeric digit.
decParser :: Parser String
decParser = do dec    <- char '.'
               digits <- digitsParser
               return (dec:digits)

-- Parses a generic, decimal number.
num :: Parser Double
num = do sign  <- optionalChar '-'
         front <- digitsParser
         back  <- optional "" decParser
         return $ read (sign ++ front ++ back)