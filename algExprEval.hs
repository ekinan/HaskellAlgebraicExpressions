module AlgExprEval
(
  evalExpr,
  evalExprFromString
) where
import AlgExpr
import AlgExprReducAddMul
import AlgExprReducDeriv
import AlgExprReducInteg

import AlgExprParser

{-
This source file contains the code for evaluating algebraic
expressions. Note that the resulting expression should not
contain the following:
  1. Any unevaluated integrals (e.g. double integrals, more
  complicated integrals such as Integr(x^2*e^x)
  
Other cases will be taken care of by the Prelude functions
(e.g. an exception will be thrown, or for divison by 0, 
a value of Infinity will be returned).

Note any derivatives encountered will be reduced first
before they are evaluated.

Trigonometric expressions are evaluated as radians.

If inconsistent behavior pops up, please try commenting
out the reduceExpr function as it has not been throughly
tested for addition and multiplication simplification.
-}

-- Top level function
evalExprFromString :: Double -> String -> Maybe Double
evalExprFromString x str 
  = ((flip evalExpr) x . reduceExpr . parseAlgExpr) str

reduceExpr :: AlgExpr -> AlgExpr
reduceExpr 
  =  simplifyAddMul . reduceIntegs . simplifyAddMul . reduceDerivs . simplifyAddMul
  
evalUnaryExpr :: (Double -> Double) 
              -> AlgExpr
              -> Double              
              -> Maybe Double
evalUnaryExpr f e x = do r <- evalExpr e x
                         return $ f r
                         
evalBinaryExpr :: (Double -> Double -> Double)
               -> AlgExpr
               -> AlgExpr
               -> Double
               -> Maybe Double
evalBinaryExpr op l r x = do lv <- evalExpr l x
                             rv <- evalExpr r x
                             return $ op lv rv

evalExpr :: AlgExpr -> Double -> Maybe Double

-- Unit values
evalExpr (Const n) _   = return n
evalExpr X x           = return x
evalExpr E x           = return $ exp 1

-- Trigonometric
evalExpr (Sin e) x     = evalUnaryExpr sin e x
evalExpr (Cos e) x     = evalUnaryExpr cos e x
evalExpr (Tan e) x     = evalUnaryExpr tan e x

-- Logarithmic 
evalExpr (Ln e) x      = evalUnaryExpr log e x
evalExpr (Log b e) x   = evalUnaryExpr ((*bFact) . log) e x
 where
  bFact = 1/(log $ fromIntegral b)
  
-- Binary Operations
evalExpr (Pow l r) x = evalBinaryExpr (**) l r x
evalExpr (Add l r) x = evalBinaryExpr (+) l r x
evalExpr (Sub l r) x = evalBinaryExpr (-) l r x
evalExpr (Mul l r) x = evalBinaryExpr (*) l r x
evalExpr (Div l r) x = evalBinaryExpr (/) l r x

-- Any remaining derivatives/integrals will cause an exception
evalExpr dv@(Deriv n e) x = evalExpr (reduceDeriv dv) x
evalExpr _ _ = Nothing