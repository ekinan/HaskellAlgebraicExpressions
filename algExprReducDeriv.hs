module AlgExprReducDeriv
(
  reduceDeriv,
  reduceDerivs
) where
import AlgExpr
import AlgExprReducUtils

{-
This source file contains the code for reducing derivatives
of the form d^ny/dx^n(f(x)). It will work for pretty much
any derivative, although the expressions will get quite
long.
-}

-- Top level function
reduceDerivs :: AlgExpr -> AlgExpr
reduceDerivs e = applyReduction isDerivExpr reduceDeriv e 

-- Helper function to use with applyReduction
isDerivExpr :: AlgExpr -> Bool
isDerivExpr (Deriv _ _) = True
isDerivExpr _            = False

-- Can't pattern match on constructors so need to
-- do this
outsideDeriv :: AlgExpr -> (AlgExpr -> AlgExpr)
outsideDeriv (Sin _)   = Cos
outsideDeriv (Cos _)   = \e -> Mul (Const (-1)) (Cos e)
outsideDeriv (Tan _)   = \e -> Div (Const 1) (Cos e)
outsideDeriv (Ln _)    = \e -> Div (Const 1) (e)
outsideDeriv (Pow X nth@(Const n))
  = \e -> Mul nth (Pow e (Const (n-1)))
outsideDeriv (Pow E _) = \e -> Mul E e


-- Applies the chain rule given the appropriate constructor.
-- Chain rule is f'(g(x)) = g'(x)*f'(g(x))
chainRule :: (AlgExpr -> AlgExpr) -> AlgExpr -> AlgExpr
chainRule f (Const n) = Const 0
chainRule f E         = Const 0
chainRule f X         = (outsideDeriv $ f X) X
chainRule f e         = Mul (reduceDeriv e) ((outsideDeriv $ f X) e)

-- Helper function for the product rule. Used to make quotient
-- rule and product rules easier.
prodRule :: (AlgExpr -> AlgExpr -> AlgExpr) 
         -> AlgExpr 
         -> AlgExpr 
         -> AlgExpr
prodRule constr f g 
  = constr (Mul (reduceDeriv f) g) (Mul f (reduceDeriv g))

-- Reduces any derivatives that are in the algebraic expression
-- tree.
reduceDeriv :: AlgExpr -> AlgExpr

-- Base case derivatives
reduceDeriv (Deriv 1 e)    = reduceDeriv e
reduceDeriv (Deriv n e)    = reduceDeriv (Deriv (n-1) (reduceDeriv e))

reduceDeriv (Const n)      = Const 0
reduceDeriv (X)            = Const 1
reduceDeriv (E)            = Const 0
reduceDeriv ex@(Pow E (X)) = ex
reduceDeriv (Sin X)        = Cos X
reduceDeriv (Cos X)        = Mul (Const (-1)) (Sin X) -- -sin(x)
reduceDeriv (Tan X)        = Div (Const 1) (Cos X) -- sec(x) = 1/cos(x)
reduceDeriv (Ln X)         = Div (Const 1) (X)
reduceDeriv (Pow (X) nth@(Const n)) = Mul nth (Pow (X) (Const (n-1)))  
reduceDeriv p@(Pow nth@(Const n) X) = Mul (Ln nth) (p)
reduceDeriv (Integr e)     = e
  
-- Chain rule cases
reduceDeriv (Sin e)        = chainRule Sin e
reduceDeriv (Cos e)        = chainRule Cos e
reduceDeriv (Tan e)        = chainRule Tan e
reduceDeriv (Ln e)         = chainRule Ln e
reduceDeriv (Log b e) -- Change of base here
  = Mul baseFact (reduceDeriv (Ln e))
 where
  baseFact = Div (Const 1) (Ln (Const $ fromIntegral b))

-- Binary operators
reduceDeriv (Pow e (Const n))
  = chainRule ((flip Pow) $ Const n) e
  
-- (f(x)^g(x))' = (e^(g(x)*ln(f(x))))'
reduceDeriv (Pow f g)
  = chainRule (Pow E) (Mul g (Ln f))  

-- Addition and Subtraction
reduceDeriv (Add l r) = Add (reduceDeriv l) (reduceDeriv r)
reduceDeriv (Sub l r) = Sub (reduceDeriv l) (reduceDeriv r)

-- Product rule: (f(x)g(x))' = f'(x)g(x) + f(x)g'(x)
-- Quotient rule: (f(x)/g(x))' = (f'(x)g(x) - f(x)g'(x))/(g(x)^2)
reduceDeriv (Mul l r) = prodRule Add l r
reduceDeriv (Div l r) = Div (prodRule Sub l r) (Pow r (Const 2))