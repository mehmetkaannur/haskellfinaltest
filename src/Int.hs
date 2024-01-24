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
addP = undefined

mulP :: Polynomial -> Polynomial -> Polynomial
mulP = undefined

sumP :: [Polynomial] -> Polynomial
sumP = undefined

prodP :: [Polynomial] -> Polynomial
prodP = undefined

diffT :: Term -> Term
diffT = undefined

intT :: Term -> Term
intT = undefined

diffP :: Polynomial -> Polynomial
diffP = undefined

intP :: Polynomial -> Polynomial
intP = undefined

-------------------------------------------------
-- Part II (7 marks)

diffE :: Expr -> Expr
diffE = undefined

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

intE :: Expr -> Maybe Expr
intE = undefined

applyICR :: Expr -> Expr -> Maybe Expr
applyICR = undefined -- you may find this useful to implement

--
-- Given...
--
simplifiedInt :: Expr -> Maybe Expr
simplifiedInt = fmap simplify . intE

printInt :: Expr -> IO ()
printInt e = maybe (putStrLn "Fail") prettyPrint (simplifiedInt e)
