module AlgExprParser
(
  parseAlgExpr
) where
import AlgExpr
import Parser
import ParserUtils

import Control.Applicative hiding (Const, optional)
import Prelude hiding (filter)

{-
This is the source file containing the parser for a single algebraic expression 
(shortened to Expr for simplicity). The grammar for algebraic expresssions is as 
follows (note that I will remove the left recursion in the implementation, this is
just simpler to understand):
  Expr          -> Expr '+' Factor
                 | Expr '-' Factor
                 | Factor
		  
	Factor        -> Factor '*' Power
                 | Factor '/' Power
                 | Power
                 
  where (p)? means that p is optional. Note that the (num)? Power part lets
  us write expressions like 55x or 27sin(x)
          
  Power         -> Power '^' DerivInteg
                 | DerivInteg
          
  DerivInteg    -> Derivative
                 | Integral
                 | Atom  

  Derivative    -> "dy/dx" '(' Expr ')'
                 | "d^" pN "y/dx" '^' pN '('  Expr ')'
                 
  Integral      -> 'I' '(' Expr ')'
                 
  where pN is a positive integer. Note in the derivative expression,
  the two pN's are meant to be equal (I know that's illegal in a grammar,
  but the parser here incorporates that).
  
	Atom          -> Unit
                 | Trigonometric
                 | Logarithmic
                 | '(' Expr ')'
          
  Unit          -> num
                 | 'x'
                 | 'e'
                
  Trigonometric -> "sin(" Expr ")"
                |  "cos(" Expr ")"
                |  "tan(" Expr ")"

  Logarithmic   -> "ln(" Expr ")"
                |  "log" (pN)? "(" Expr ")"
-}

-- IMPLEMENTATION FOR PARSING POWER, FACTORS AND EXPR

-- Parses out an algebraic expression from the given string
parseAlgExpr :: String -> AlgExpr
parseAlgExpr str = res
 where
  (Just (filtStr, _)) = runParser (filterChar ' ') str -- Filter out whitespace
  (Just (res, []))     = runParser algExpr filtStr
  
-- Top level parser
algExpr  = leftRecur [factor] [addPart, subPart]
 where
  addPart = binExprIP '+' Add factor
  subPart = binExprIP '-' Sub factor
  
-- Parser for multiplying and dividing expressions (Factor nonterminal)
factor = leftRecur [power] [mulPart, divPart]
 where
  mulPart  = binExprIP '*' Mul power
  divPart  = binExprIP '/' Div power
  
  numAndPow = liftA2 Mul aNum power
  
-- Parser for the Power non-terminal
power = leftRecur [derivInteg] [binExprIP '^' Pow derivInteg]

-- Helper method to create the (op p) part in the left
-- recursion of the AlgExpr grammar, to use with
-- the generic leftRecur method. It creates a parser
-- that has a partially applied constructor (the right
-- half in the binary operation's AST).
binExprIP :: Char 
         -> (AlgExpr -> AlgExpr -> AlgExpr) 
         -> Parser AlgExpr 
         -> Parser (AlgExpr -> AlgExpr)
binExprIP op opConst p = (flip opConst) <$> (char op *> p)

-- Returns a new parser that eliminates any left recursion
-- in our expression grammar. Takes as input two lists, the 
-- terminating parsers and the intermediate parsers. For 
-- example if we have something of the form 
-- E -> E '+' F | E '-' F | F then the terminating parser 
-- would be [F], and the intermediate parsers would be 
-- [('+' F), ('-' F)] 
leftRecur :: [Parser AlgExpr] -> [Parser (AlgExpr -> AlgExpr)] -> Parser AlgExpr
leftRecur ts is = do tr <- alt ts
                     irs <- zeroOrMore $ alt is
                     return $ foldl combine tr irs
 where
  combine e const = const e
  
-- IMPLEMENTATION FOR PARSING DERIVATIVES AND INTEGRALS

-- Parser for derivatives and integrals
derivInteg :: Parser AlgExpr
derivInteg = deriv <|> integ <|> atom

deriv :: Parser AlgExpr
deriv = singleDeriv <|> genDeriv
 where
  singleDeriv = unaryExprParser "dy/dx" (Deriv 1) algExpr
  genDeriv = do (string "d^")
                n <- posInt
                unaryExprParser ("y/dx^" ++ (show n)) (Deriv n) algExpr

integ :: Parser AlgExpr
integ = unaryExprParser "I" (Integr) algExpr


-- IMPLEMENTATION FOR ATOM PARSER BELOW

-- Parser for the atoms in our language
atom :: Parser AlgExpr
atom = unit <|> trig <|> logth <|> (parenthParser algExpr)

aNum :: Parser AlgExpr
aNum = Const <$> num

-- Parses out the Unit nonterminal
unit :: Parser AlgExpr
unit = aNum <|> x <|> e
 where
  x    = char 'x' *> pure X
  e    = char 'e' *> pure E
  
-- Trigonometric Expressions
trig :: Parser AlgExpr
trig = sin <|> cos <|> tan
 where
  sin = unaryExprParser "sin" Sin algExpr
  cos = unaryExprParser "cos" Cos algExpr
  tan = unaryExprParser "tan" Tan algExpr

-- Logarithmic expressions
logth :: Parser AlgExpr
logth = ln <|> log
 where
  ln = unaryExprParser "ln" Ln algExpr
  log = do (string "log")
           b <- optional 10 posInt
           unaryExprParser "" (Log b) algExpr 
           
-- Creates a parser for a unary operation in this language.
-- Specifically, to parse something of the form opName(AlgExpr)
unaryExprParser :: String 
                -> (AlgExpr -> AlgExpr) 
                -> Parser AlgExpr 
                -> Parser AlgExpr
unaryExprParser opName opCons p 
  = opCons <$> (string opName *> parenthParser p)
           
-- If s is a string that can be parsed by our parser p,
-- parenthParser creates a parser that parses strings of the form
-- "(s)"
parenthParser :: Parser AlgExpr -> Parser AlgExpr
parenthParser p = (char '(' *> p) <* char ')'