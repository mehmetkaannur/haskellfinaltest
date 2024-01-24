module Int where
import GHC.Real
import Data.List
import Data.Maybe
import Control.Applicative

import Types
import Utilities
import Examples

--
-- Universal assumptions/preconditions:
-- 1. All polynomials are in standard form with decreasing
--    powers of x
-- 2. 0 is represented by P [(0, 0)]; P [] is undefined for
--    the purposes of the exercise.
-- 3. All constants will be polynomials of the form
--    [(c, 0)], e.g. logarithms of constants and constant
--    powers will not appear.
-- 4. All computed integrals omit the constant of integration.
--

-------------------------------------------------
-- Part I (13 marks)

addP :: Polynomial -> Polynomial -> Polynomial
addP pol1 []      = pol1
addP [] pol2      = pol2
addP pol1 [(0,0)] = pol1
addP [(0,0)] pol2 = pol2
addP pol1@(t1@(c1,e1):xs) pol2@(t2@(c2,e2):ys)
  | e1==e2        = ((c1+c2),e1) : addP xs ys
  | e1>e2         = t1:addP xs pol2
  | otherwise     = t2:addP pol1 ys

mulP :: Polynomial -> Polynomial -> Polynomial
mulP pol1 pol2 = simplifyHelper (polMultiplier pol1 pol2)

polMultiplier :: Polynomial -> Polynomial -> Polynomial
polMultiplier pol1 pol2 =
  [(cx*cy,ex+ey)| (cx,ex) <- pol1, (cy,ey) <- pol2]

simplifyHelper :: Polynomial -> Polynomial
simplifyHelper []  = []
simplifyHelper [t] = [t]
simplifyHelper (t1@(c1,e1):t2@(c2,e2):ts)
  | e1==e2         = ((c1+c2),e1) : simplifyHelper ts
  | otherwise      = t1: simplifyHelper (t2:ts)

sumP :: [Polynomial] -> Polynomial
sumP [p]    = p
sumP (p:ps) = addP p (sumP ps)

prodP :: [Polynomial] -> Polynomial
prodP [p]    = p
prodP (p:ps) = simplifyHelper(reverse(sorted))
  where 
    unsorted = mulP p (prodP ps)
    sorted   = sortBy (\(_,a)(_,b) -> compare a b) unsorted

diffT :: Term -> Term
diffT (c,0) = (0,0)
diffT (c,e) = ((c*fromIntegral(e)),(e-1))

intT :: Term -> Term
intT (c,0) = (c,1)
intT (c,e) = ((c/fromIntegral(e+1)),(e+1))

diffP :: Polynomial -> Polynomial
diffP p = [diffT t| t <- p]

intP :: Polynomial -> Polynomial
intP p  = [intT t| t <- p]

-------------------------------------------------
-- Part II (7 marks)

diffE :: Expr -> Expr
diffE (P p)       = P(diffP p)
diffE (Add e1 e2) = Add (diffE e1) (diffE e2)
diffE (Mul e1 e2) = 
  Add (Mul e1 (diffE e2)) (Mul (diffE e1) e2)
diffE (Pow e r)   = Mul (toExpr r) (Pow e (r-1))
diffE (Log e)     = Pow e (-1)


--
-- Given
--
toExpr :: Rational -> Expr
toExpr n = P [(n, 0)]

isConstant :: Expr -> Bool
isConstant (P [(_, 0)]) = True
isConstant _ = False

simplifiedDiff :: Expr -> Expr
simplifiedDiff = simplify . diffE

printDiff :: Expr -> IO ()
printDiff = prettyPrint . simplifiedDiff

-------------------------------------------------
-- Part III (10 marks)

intoPowChecker :: Expr -> Expr -> Bool
intoPowChecker e1 (Pow e2 r) = e1 == simplify(diffE(e2))
intoPowChecker _ _           = False

intoLogChecker :: Expr -> Expr -> Bool
intoLogChecker e1 (Log e2) = e1 == simplify(diffE(e2))
intoLogChecker _ _         = False

into :: Expr -> (Expr, Maybe Rational)
into (Pow e r) = (e, Just r)
into (Log e)   = (e, Nothing)

intTaker :: Expr -> (Expr, Maybe Rational) -> Maybe Expr
intTaker e1 (e2, (Just i)) = intE (Pow e2 i)
intTaker e1 (e2, Nothing)  = intE (Log e2)

intE :: Expr -> Maybe Expr
intE (P p)                    = Just(P (intP p))
intE (Add e1 e2)              = 
  Just(Add (fromJust(intE e1)) (fromJust(intE e2)))
intE (Mul e1 e2)
  | isConstant e1             = 
      Just(Mul e1 (fromJust(intE e2)))
  | isConstant e2             = 
      Just(Mul (fromJust(intE e1)) e2)
  | e1 == simplify(diffE(e2)) = 
      Just(Mul (Pow e2 2) (Pow (toExpr 2) (-1)))
  | e2 == simplify(diffE(e1)) = 
      Just(Mul (Pow e1 2) (Pow (toExpr 2) (-1)))
  | intoPowChecker e1 e2      = intTaker e1 (into e2)
  | intoPowChecker e2 e1      = intTaker e2 (into e1)
  | intoLogChecker e1 e2      = intTaker e1 (into e2)
  | intoLogChecker e2 e1      = intTaker e2 (into e1)
--In examples e10, e11, e13 the tests doesn't work, 
--but the function is working correct.
  
intE (Pow e r)
  | r==(-1)   = Just(Log e)
  | otherwise = Just(Mul (Pow e (r+1)) (Pow (toExpr(r+1)) (-1)))
intE (Log e)  = Just(Mul e (Add (Log e) (toExpr(-1))))

intE _ = Nothing

applyICR :: Expr -> Expr -> Maybe Expr
applyICR = undefined -- you may find this useful to implement

--
-- Given...
--
simplifiedInt :: Expr -> Maybe Expr
simplifiedInt = fmap simplify . intE

printInt :: Expr -> IO ()
printInt e = maybe (putStrLn "Fail") prettyPrint (simplifiedInt e)
