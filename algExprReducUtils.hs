module AlgExprReducUtils
(
  applyReduction,
  cleanUp
) where
import AlgExprParser
import Parser
import AlgExpr

{-
This is the source file containing some helpful utility functions
used when reducing expressions that will be shared across all files.
-}

{-
Cleans up a tree after it's been reduced, removing redundancies
like multiplying a ton of 0s, dividing by 1, multiplying by 1,
etc. Keeps doing it until we can't clean up the tree anymore.
-}
cleanUp :: AlgExpr -> AlgExpr
cleanUp e
 | e ~= er = e -- Result is stable
 | otherwise = cleanUp er -- Probably more idiomatic way to do this..
 where
  er = cleanUpHelper e
  
  cleanUpHelper :: AlgExpr -> AlgExpr
  -- Trigonometric Expressions 
  cleanUpHelper (Sin e)           = Sin (cleanUpHelper e)
  cleanUpHelper (Cos e)           = Cos (cleanUpHelper e)
  cleanUpHelper (Tan e)           = Tan (cleanUpHelper e)

  -- Logarithmic
  cleanUpHelper (Ln E)            = Const 1
  cleanUpHelper (Ln e)            = Ln (cleanUpHelper e)
  cleanUpHelper expr@(Log b (Const n)) 
    | (fromIntegral b == n) = Const 1
    | otherwise             = expr
  cleanUpHelper (Log b e)         = Log b (cleanUpHelper e)

  -- Differentiation and Integration
  cleanUpHelper (Deriv n e)       = Deriv n (cleanUpHelper e)
  cleanUpHelper (Integr e)        = Integr (cleanUpHelper e)

  -- Binary Expressions
  cleanUpHelper (Pow e (Const 0)) = (Const 1)
  cleanUpHelper (Pow e (Const 1)) = cleanUpHelper e
  cleanUpHelper (Pow e1 e2)       = Pow (cleanUpHelper e1) (cleanUpHelper e2)

  cleanUpHelper (Add e (Const 0)) = cleanUpHelper e
  cleanUpHelper (Add (Const 0) e) = cleanUpHelper e
  cleanUpHelper (Add e1 e2)       = Add (cleanUpHelper e1) (cleanUpHelper e2)

  cleanUpHelper (Sub e (Const 0)) = cleanUpHelper e
  cleanUpHelper (Sub e1 e2)       = Sub (cleanUpHelper e1) (cleanUpHelper e2)

  cleanUpHelper (Mul e (Const 0)) = Const 0
  cleanUpHelper (Mul (Const 0) e) = Const 0
  cleanUpHelper (Mul e (Const 1)) = cleanUpHelper e
  cleanUpHelper (Mul (Const 1) e) = cleanUpHelper e
  cleanUpHelper (Mul e1 e2)       = Mul (cleanUpHelper e1) (cleanUpHelper e2)

  cleanUpHelper (Div e (Const 1)) = cleanUpHelper e
  cleanUpHelper (Div (Const 0) e) = (Const 0)
  cleanUpHelper (Div e1 e2)       = Div (cleanUpHelper e1) (cleanUpHelper e2)

  cleanUpHelper e = e -- Base case

{-
This method takes some kind of node identifier, and a reduction for
that node, and applies it to the AST. It is used in differentiating,
integrating, and combining like terms (as only certain nodes will
be affected, while the others will propagate their actions to
their children).
-}
applyReduction :: (AlgExpr -> Bool)
               -> (AlgExpr -> AlgExpr)
               ->  AlgExpr
               ->  AlgExpr
applyReduction reducible action e
  | reducible e = cleanUp $ action e
  | otherwise = cleanUp $ propagate e
 where
  reduceSubtree e = applyReduction reducible action e
  
  -- Unit
  propagate c@(Const _) = c
  propagate X           = X
  propagate E           = E
  
  -- Trigonometric
  propagate (Sin e)     = Sin (reduceSubtree e)
  propagate (Cos e)     = Cos (reduceSubtree e)
  propagate (Tan e)     = Tan (reduceSubtree e)
  
  -- Logarithmic
  propagate (Ln e)      = Ln (reduceSubtree e)
  propagate (Log b e)   = Log b (reduceSubtree e)
  
  -- Differentiation and Integration
  propagate (Deriv n e) = Deriv n (reduceSubtree e)
  propagate (Integr e)  = Integr (reduceSubtree e)
  
  -- Binary Operations
  propagate (Pow e1 e2) = Pow (reduceSubtree e1) (reduceSubtree e2)
  propagate (Add e1 e2) = Add (reduceSubtree e1) (reduceSubtree e2)  
  propagate (Sub e1 e2) = Sub (reduceSubtree e1) (reduceSubtree e2)  
  propagate (Mul e1 e2) = Mul (reduceSubtree e1) (reduceSubtree e2)
  propagate (Div e1 e2) = Div (reduceSubtree e1) (reduceSubtree e2)