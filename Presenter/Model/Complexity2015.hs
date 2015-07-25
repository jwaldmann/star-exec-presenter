-- | syntax and semantics see
-- http://cl-informatik.uibk.ac.at/users/georg/cbr/competition/rules.php
--
-- * the scoring introduces an artificial total order
-- (but actually it is about sets of functions,
-- which are partially ordered by inclusion in a natural way)

module Presenter.Model.Complexity2015 where

import Presenter.Short
import qualified Data.Text as T
import Prelude 

import Text.ParserCombinators.ReadP
import Data.Char ( isDigit )
import Control.Applicative 

-- | NOTE: Ord instance is required for keys in maps (?)
-- but otherwise useless
data Bounds = Bounds { lower :: Function
                     , upper :: Function
                     }
     deriving (Eq, Ord)

-- | test all branches of the original grammar:
b1 = Bounds { lower = Infinite, upper = Infinite } -- MAYBE
b2 = Bounds { lower = Finite, upper = Infinite } -- NON_POLY lower bound
b3 = Bounds { lower = Poly $ Just 2, upper = Finite } -- termination is known
b4 = Bounds { lower = Poly $ Just 2, upper = Infinite } -- termination is not known
b5 = Bounds { lower = Poly $ Just 1, upper = Poly $ Just 3 } 
b6 = Bounds { lower = Finite , upper = Poly $ Nothing }

instance Show Bounds where 
    showsPrec p b = 
        let parens c = ("(" ++) . c . (")" ++)
            ( par, prefix, interesting) = case (lower b, upper b) of
               ( Infinite , Infinite ) -> (False, "MAYBE", False)
               ( l, u ) -> (True, "WORST_CASE", True ) -- l is at least FINITE here
            maybe_parens c = if par && p > 0 then parens c else c
            out = prefix ++ if interesting
                      then "(" ++ showLower (lower b) ++ "," ++ showUpper (upper b) ++")"
                      else ""   
        in  maybe_parens (out ++)

instance Short Bounds where
  short b =
    let sslower f = case f of
          Poly Nothing -> "n^?" ; Poly (Just d) -> "n^" ++ show d 
          Finite -> "?" ; Infinite -> "-"
        ssupper f = case f of
          Poly Nothing -> "n^?" ; Poly (Just d) -> "n^" ++ show d 
          Finite -> "-" ; Infinite -> "?"
    in  T.pack $ sslower (lower b) ++ "/" ++ ssupper (upper b)

instance Read Bounds where
    readsPrec _ = readP_to_S $ skipSpaces *> readP_Bounds


readP_Bounds = parens readP_Bounds_bare +++ readP_Bounds_bare

readP_Bounds_bare = 
        do { token "WORST_CASE" ; pair Infinite Infinite }
    +++ do { token "MAYBE" ; pair Infinite Infinite }

pair lo up = 
    parens ( do l <- quest lo readP_FunctionL 
                token "," 
                u <- quest up readP_FunctionU 
                return $ Bounds { lower = l, upper = u } )
    <++ return ( Bounds { lower = lo, upper = up } )

quest q p = do { token "?" ; return q } +++ p

-- | NOTE: Ord instance is required for keys in maps (?)
-- but otherwise useless
data Function = Poly { degree :: Maybe Int } -- ^ nothing: unknown degree
              | Finite -- ^ some unspecified function (each value is finite)
              | Infinite -- ^ some function that has infinite values somewhere
     deriving (Eq, Ord)

-- * comparison of functions for bounds.
-- need to know the context (is it for upper or lower bounds)
-- since we need to treat polynomials with unknown degree accordingly:
-- for an upper bound, an unknown polyomial is considered larger than any known,
-- while for lower bounds, it is considerer smaller than any known polynomial.

compare_for_lower_bounds :: Function -> Function -> Ordering
compare_for_lower_bounds f g = case (f,g) of
  ( _ , _ ) | f == g -> EQ
  ( Infinite, _ ) -> GT
  ( _ , Infinite ) -> LT
  ( Finite, _ ) -> GT
  ( _ , Finite ) -> LT
  ( Poly (Just _), Poly Nothing ) -> GT
  ( Poly Nothing, Poly (Just _) ) -> LT
  ( Poly (Just p), Poly (Just q)) -> compare p q

compare_for_upper_bounds :: Function -> Function -> Ordering
compare_for_upper_bounds f g = case (f,g) of
  ( _ , _ ) | f == g -> EQ
  ( Infinite, _ ) -> GT
  ( _ , Infinite ) -> LT
  ( Finite, _ ) -> GT
  ( _ , Finite ) -> LT
  ( Poly Nothing, Poly (Just _) ) -> GT
  ( Poly (Just _), Poly Nothing ) -> LT
  ( Poly (Just p), Poly (Just q)) -> compare p q


isPoly f = case f of Poly {} -> True ; _ -> False

readP_FunctionL = do { token "NON_POLY" ; return $ Finite }
    +++ do { token "Omega" ; parens $ ( Poly . Just ) <$> readP_degreeL }

readP_degreeL =
  do { token "n" ; token "^"
     ; ds <- many1 $ satisfy isDigit ; skipSpaces
     ; return $ foldl ( \ n d -> 10*n + fromEnum d - fromEnum '0' ) 0 ds 
     }

readP_FunctionU = do { token "POLY" ; return $ Poly $ Nothing }
    +++ do { token "O" ; parens $ ( Poly . Just ) <$> readP_degreeU }

readP_degreeU = do { token "1" ; return 0 }
    +++ readP_degreeL

token s = do string s ; skipSpaces

parens p = between (token "(") (token ")") p

showLower :: Function -> String
showLower f = case f of
  Poly {} -> case degree f of
    Nothing -> "POLY"
    Just d -> "Omega(n^" ++ show d ++ ")"
  Finite -> "NON_POLY"
  Infinite -> "?"

showUpper :: Function -> String
showUpper f = case f of
  Poly {} -> case degree f of
    Nothing -> "POLY"
    Just d -> case d of
      0 -> "O(1)"
      k -> "O(n^" ++ show k ++ ")"
  Finite -> "?"
  Infinite -> "?"
