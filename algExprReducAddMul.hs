module AlgExprReducAddMul
(
  simplifyAddMul,
  simplifyAddExpr,
  simplifyMulExpr
) where
import AlgExpr
import AlgExprReducUtils

import Data.List

{-
This is the source file containing the interpreter code for simplifying 
addititive and multiplicative expressions by combining like terms
together. Note that subtraction, division and power terms are not
done here, but they could readily be added given the AlgExprReducUtils
framework.
-}

-- Top level method.
simplifyAddMul = simplifyAddExpr . simplifyMulExpr
  
{-
This should be called with binary expressions (e.g.
addition and multipliation). Then flatten e will
never return an empty list. In any case, this method,
simplifies an algebraic expression by combining its
like terms. Currently, it's used for addition and
multiplication, but it could be used for other
operations too. Note that if the new tree isn't
different from the old one (i.e. reduction doesn't
do anything, then we just return the old tree)
-}
simplifyExpr :: (AlgExpr -> Bool)
             -> (AlgExpr -> AlgExpr -> Bool)
             -> (AlgExpr -> [AlgExpr])
             -> (AlgExpr -> AlgExpr -> AlgExpr)
             -> ([AlgExpr] -> AlgExpr)
             -> AlgExpr
             -> AlgExpr
simplifyExpr exprKind likeTerms flatten merge build e
  = build mergedEs
  where
   subSimpl = applyReduction exprKind (simplifyExpr exprKind likeTerms flatten merge build)
          
    -- Get all of the relevant expressions, simplify them first.
   (fe:(fes)) = map subSimpl $ flatten e
   
   mergeLikes mes e = res
    where
     likes = filter (likeTerms e) mes
     mergedE = foldl merge e likes
     res = mergedE:(filter (not . likeTerms e) mes)
  
   mergedEs = foldl mergeLikes [fe] fes
   
{-
Simplifies multiplication expressions (e.g. those of the form
f1(x)*f2(x)*...fn(x).)
-}
simplifyMulExpr :: AlgExpr -> AlgExpr
simplifyMulExpr = applyReduction isMulExpr simplify
 where
  isMulExpr (Mul _ _) = True
  isMulExpr _ = False
 
  -- Represents x^(f(x)), x^(g(x))
  likeTerms (Pow e1 _) (Pow e2 _) = e1 ~= e2
  -- Represents x, x^(f(x))
  likeTerms e1 (Pow e2 _) = e1 ~= e2
  likeTerms (Pow e2 _) e1 = e1 ~= e2
  
  -- Represents x and x
  likeTerms e1 e2 = e1 ~= e2

  flatten (Mul e1 e2) = flatten e1 ++ flatten e2
  flatten e = [e]  
  
  -- Represents 5*3 = 15, 4*3 = 12
  merge (Const x) (Const y) = Const (x*y)
  
  -- Represents x^m*x^n = x^(m+n)
  merge (Pow e (Const n)) (Pow e' (Const m)) = Pow e (Const (n+m))

  -- Represents x^f(x)*x^(g(x)) = x^(f(x)+g(x))  
  merge (Pow e f) (Pow e' g) = Pow e (Add f g) 
  
  -- Represents x*x^(n) = x^(n+1), x^(n)*x = x^(n+1)
  merge e (Pow e' (Const n)) = Pow e (Const (n + 1))
  merge p@(Pow e' (Const n)) e = merge e p

  -- Represents x*x^(f(x)) = x^(1 + f(x)), x^(f(x))*x = x^(1+f(x)) 
  merge e (Pow e' f) = Pow e (Add (f) (Const 1))
  merge p@(Pow e' f) e = merge e p
  
  -- Represents x*x = x^2
  merge e e' = Pow e (Const 2)
  
  -- Combines the simplified expressions together.
  build [e] = e
  build es = foldl1 Mul es
  
  simplify 
    = simplifyExpr isMulExpr likeTerms flatten merge build
  
{-
Simplifies additive expressions (e.g. those of the form
f1(x)+f2(x)+...fn(x).)
-}
simplifyAddExpr :: AlgExpr -> AlgExpr
simplifyAddExpr = applyReduction isAddExpr simplify
 where
  isAddExpr (Add _ _) = True
  isAddExpr _ = False
 
  -- Represents m*x+n*x, a lot of this is boilerplate due to
  -- the commutativity of multiplication (ugh)
  likeTerms (Mul (Const _) e1) (Mul (Const _) e2) = e1 ~= e2
  likeTerms (Mul e1 (Const _)) (Mul (Const _) e2) = e1 ~= e2
  likeTerms (Mul (Const _) e1) (Mul e2 (Const _)) = e1 ~= e2
  likeTerms (Mul e1 (Const _)) (Mul e2 (Const _)) = e1 ~= e2
  
  -- Represents x + n*x
  likeTerms e1 (Mul (Const _) e2) = e1 ~= e2
  likeTerms e1 (Mul e2 (Const _)) = e1 ~= e2
  likeTerms (Mul (Const _) e2) e1 = e1 ~= e2
  likeTerms (Mul e2 (Const _)) e1 = e1 ~= e2
  
  -- Represents x + x
  likeTerms e1 e2 = e1 ~= e2

  flatten (Add e1 e2) = flatten e1 ++ flatten e2
  flatten e = [e]  
  
  -- Represents something like 3 + 6
  merge (Const m) (Const n) = Const (m + n)
  
  -- Represents m*x+n*x = (m+n)*x
  merge (Mul (Const m) e1) (Mul (Const n) e2) 
    = (Mul (Const $ m + n) e1)
  merge (Mul e1 (Const m)) (Mul (Const n) e2)
    = (Mul (Const $ m + n) e1)
  merge (Mul (Const m) e1) (Mul e2 (Const n))
    = (Mul (Const $ m + n) e1)
  merge (Mul e1 (Const m)) (Mul e2 (Const n))
    = (Mul (Const $ m + n) e1)
    
  -- Represents x + n*x
  merge e1 (Mul (Const m) e2) 
    = (Mul (Const $ m + 1) e1)
  merge e1 (Mul e2 (Const m))  
    = (Mul (Const $ m + 1) e1)
  merge (Mul (Const m) e2) e1 
    = (Mul (Const $ m + 1) e1)
  merge (Mul e2 (Const m)) e1 
    = (Mul (Const $ m + 1) e1)
    
  -- Represents x + x
  merge e1 e2 = Mul (Const 2) e1 
  
  -- Combines the simplified expressions together.
  build [e] = e
  build es = foldl1 Add es
  
  simplify 
    = simplifyExpr isAddExpr likeTerms flatten merge build 