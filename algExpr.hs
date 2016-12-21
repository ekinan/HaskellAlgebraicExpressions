module AlgExpr
(
  AlgExpr(..),
  (~=)
) where
{-
This is the source file containing all of the possible algebraic expressions.
Currently, the language will have support for the following:
    1. Numeric constants (e.g. 1, 2, 3, -5, -6)
    2. Trigonometric (sin(x), cos(x), tan(x))
    3. Logarithmic (ln(x), logb(x))
    4. Basic Differentiation
    5. Basic Integration
    6. Combination of any expressions in 1-5. These include the following:
           a. Addition
           b. Subtraction
           c. Multiplication
           d. Division
           e. Exponentiation (e.g. sin(x)^(cos(x)))
           f. Composition (e.g. sin(sin(x)), ln(x^2+5), etc.)

    NOTE: Polynomials are taken care of implicitly by exponentiation,
    same with expressions such as e^(x) and b^(x)
           
       The precedence ordering is as follows (highest to lowest):
          1. Composition
          3. Differentiation and Integration
          4. Exponentiation (groups to the left)
          5. Multiplication and Division (groups to the left)
          6. Adddition and Subtraction (groups to the left)
-}

-- Main algebraic expression datatype.
data AlgExpr =
          -- Fundamental units 
            Const Double           -- Constant expressions, such as "1.5", "2", "3"
          | X                      -- Represents a single "x"
          | E                      -- Represents the exponential constant E
          
          -- Trigonometric
          | Sin AlgExpr            -- Represents sin(x)
          | Cos AlgExpr            -- Represents cos(x)
          | Tan AlgExpr            -- Represents tan(x)
          
          -- Logarithmic
          | Ln AlgExpr             -- Represents ln(x)
          | Log Int AlgExpr        -- Represents logb(x)
          
          -- Differentiation and Integration
          | Deriv Int AlgExpr      -- Represents d^ny/dx^n(f(x))
          | Integr AlgExpr         -- Represents I(f(x)) 

          -- Binary Operations       
          | Pow AlgExpr AlgExpr    -- Expressions such as x^2, x^(sin(x)), etc.
          | Add AlgExpr AlgExpr    -- Addition of two expressions
          | Sub AlgExpr AlgExpr    -- Subtraction of two expressions
          | Mul AlgExpr AlgExpr    -- Multiplication of two expressions
          | Div AlgExpr AlgExpr    -- Division of two expressions
  deriving(Eq)
 
{-
I apologize in advance for all the boiler plate code
below, but I did not know of a better way to do this
(Data.Data looked promising, but I did not want to 
spend too much time learning more advanced Haskell
concepts and avoid actually doing the features in
the language).
-}

-- Shows unary expressions such as sin(f(x)), ln(f(x))
showUnaryExpr :: String -> AlgExpr -> String
showUnaryExpr f e = f ++ "(" ++ show e ++ ")"


-- Show derivative expressions
showDerivExpr :: Int -> AlgExpr -> String
showDerivExpr 1 e = showUnaryExpr "dy/dx" e
showDerivExpr n e = showUnaryExpr ("d^" ++ show n ++ "y/dx^" ++ show n) e
          
-- Shows binary expressions such as (x + 5)
showBinExpr :: String -> AlgExpr -> AlgExpr -> String
showBinExpr op l r = "(" ++ show l ++ op ++ show r ++ ")"           

-- Instance of the Show class for algebraic expressions
instance Show AlgExpr where
  -- Fundamental units
  show (Const n)     = show n
  show (X)           = "x"
  show (E)           = "e"
  
  -- Trigonometric
  show (Sin e)       = showUnaryExpr "sin" e
  show (Cos e)       = showUnaryExpr "cos" e
  show (Tan e)       = showUnaryExpr "tan" e
  
  -- Logarithmic
  show (Ln e)        = showUnaryExpr "ln"  e
  show (Log 10 e)    = showUnaryExpr "log" e
  show (Log b e)     = showUnaryExpr ("log" ++ show b) e
  
  -- Differentiation and Integration
  show (Deriv n e)   = showDerivExpr n e
  show (Integr e)    = showUnaryExpr "I" e
  
  -- Binary operations
  show (Pow l r)     = showBinExpr "^" l r
  show (Add l r)     = showBinExpr "+" l r
  show (Sub l r)     = showBinExpr "-" l r
  show (Mul l r)     = showBinExpr "*" l r
  show (Div l r)     = showBinExpr "/" l r
  
  
-- Operator for AlgExprs that classifies structurally
-- equivalent terms (i.e. same AST). Pretty much the
-- same as ==, except that it will take advantage of
-- commutative operations
(~=) :: AlgExpr -> AlgExpr -> Bool


(Const _) ~= (Const _)         = True

(Add ae1 ae2) ~= (Add be1 be2) = ((ae1 == be1) && (ae2 == be2))
                              || ((ae2 == be1) && (ae1 == be2))
                                 
(Mul ae1 ae2) ~= (Mul be1 be2) = ((ae1 == be1) && (ae2 == be2))
                              || ((ae2 == be1) && (ae1 == be2))

e1 ~= e2                       = e1 == e2