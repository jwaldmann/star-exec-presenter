-- | syntax and semantics see
-- http://cl-informatik.uibk.ac.at/users/georg/cbr/competition/rules.php
--
-- * the scoring introduces an artificial total order
-- (but actually it is about sets of functions,
-- which are partially ordered by inclusion in a natural way)

module Presenter.Model.Complexity2015 where

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
    show b = 
        let ( prefix, interesting) = case (lower b, upper b) of
               ( Infinite , Infinite ) -> ("MAYBE", False)
               ( l, u ) -> ("WORST_CASE", True ) -- l is at least FINITE here
        in  prefix ++ if interesting
                      then "(" ++ showLower (lower b) ++ "," ++ showUpper (upper b) ++")"
                      else ""

instance Read Bounds where
    readsPrec _ = readP_to_S readP_Bounds


readP_Bounds = do { token "WORST_CASE" ; pair Infinite Infinite }
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
data Function = Poly { degree :: Maybe Int } 
              | Finite 
              | Infinite 
     deriving (Eq, Ord)

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
