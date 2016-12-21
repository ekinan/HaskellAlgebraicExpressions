module AlgExprReducInteg
(
  reduceIntegs
) where
import AlgExpr
import AlgExprReducUtils

{-
This source file contains all of the rules needed to evaluate
an integral. It is a very basic implementation, and does not take
into account the "+C" term. It is mostly intended to just compute
basic anti-derivatives.
-}

-- Top level function
reduceIntegs :: AlgExpr -> AlgExpr
reduceIntegs e = applyReduction isIntegExpr reduceInteg e 

isIntegExpr :: AlgExpr -> Bool
isIntegExpr (Integr _) = True
isIntegExpr _          = False

-- Reduces any integrals that are in the algebraic expression
-- tree, if possible
reduceInteg :: AlgExpr -> AlgExpr


-- Base case integrals
reduceInteg (Integr e)     = reduceInteg e

reduceInteg (Deriv 1 e)    = e
reduceInteg (Deriv n e)    = (Deriv (n-1) (e))

reduceInteg c@(Const n)      = Mul c X 
reduceInteg (X)              = Div (Pow X (Const 2)) (Const 2)

-- E cases
reduceInteg (E)              = Mul E X
reduceInteg ex@(Pow E (X))   = ex
reduceInteg ex@(Pow E nth@(Const n))
  = Mul ex X
  
-- Unary function cases
reduceInteg (Sin X)          = Mul (Const (-1)) (Cos X)
reduceInteg (Cos X)          = Sin X
reduceInteg (Tan X)          = Mul (Const (-1)) (Ln (Cos X)) -- Don't have abs. value
reduceInteg (Ln X)           = Sub (Mul X (Ln X)) (X)
reduceInteg (Pow (X) nth@(Const n)) 
  = Div (Pow (X) (Const (n+1))) (Const (n+1))  
reduceInteg p@(Pow nth@(Const n) X) = Div p (Ln nth) 

-- Addition and Subtraction
reduceInteg (Add l r) = Add (reduceInteg l) (reduceInteg r)
reduceInteg (Sub l r) = Sub (reduceInteg l) (reduceInteg r)

reduceInteg (Mul nth@(Const n) r) = Mul nth (reduceInteg r)
reduceInteg (Mul l nth@(Const n)) = Mul nth (reduceInteg l)

reduceInteg (Div l nth@(Const n)) = Div (reduceInteg l) nth

-- Remaining cases, can't evaluate.
reduceInteg e = e