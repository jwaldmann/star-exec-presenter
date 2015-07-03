-- | syntax and semantics see
-- http://cl-informatik.uibk.ac.at/users/georg/cbr/competition/rules.php

-- there are several problems with this definition:
-- * it is wrong (uses big-oh for lower bounds, instead of big-omega)
-- * it is not expressible enough (it cannot express "at least quadratic, but not known to be terminating")
-- * the scoring introduces an artificial total order (but actually it is about sets of functions,
--   which are partially ordered by inclusion in a natural way)

module Presenter.Model.Complexity where

import Prelude
import Text.ParserCombinators.ReadP
import Data.Char ( isDigit )
import Control.Applicative 

data Bounds = Bounds
  { lower :: Function
  , upper :: Function
  } deriving ( Eq )

-- | test all branches of the original grammar:
b1 = Bounds { lower = Infinite, upper = Infinite } -- NO
b2 = Bounds { lower = Finite, upper = Infinite } -- an actual MAYBE
b3 = Bounds { lower = Poly $ Just 2, upper = Finite } -- termination is known
b4 = Bounds { lower = Poly $ Just 2, upper = Infinite } -- termination is not known
b5 = Bounds { lower = Poly $ Just 1, upper = Poly $ Just 3 } 
b6 = Bounds { lower = Finite , upper = Poly $ Nothing }

instance Show Bounds where 
  show b = 
    let ( prefix, interesting) = case (lower b, upper b) of
          ( Infinite , _ ) -> ("NO", False)
          ( l, u ) -> case u of
            Infinite ->  ("MAYBE" , l /= Finite )
            _ -> ("YES", l /= Finite || u /= Finite )
    in  prefix ++ if interesting then show (lower b, upper b) else ""

instance Read Bounds where
    readsPrec _ = readP_to_S readP_Bounds

readP_Bounds :: ReadP Bounds
readP_Bounds = do { token "YES" ; pair Finite Finite }
    +++ do { token "MAYBE" ; pair Finite Infinite }
    +++ do { token "NO" ; pair Infinite Infinite }

pair :: Function -> Function -> ReadP Bounds
pair lo up = 
    parens ( do l <- quest lo readP_Function 
                token "," 
                u <- quest up readP_Function 
                return $ Bounds { lower = l, upper = u } )
    <++ return ( Bounds { lower = lo, upper = up } )

quest :: a -> ReadP a -> ReadP a
quest q p = do { token "?" ; return q } +++ p

data Function =
  Poly { degree :: Maybe Int } 
  | Finite 
  | Infinite 
  deriving ( Eq )

isPoly :: Function -> Bool
isPoly f = case f of Poly {} -> True ; _ -> False

readP_Function :: ReadP Function
readP_Function = do { token "POLY" ; return $ Poly $ Nothing }
    +++ do { token "O" ; parens $ ( Poly . Just ) <$> readP_degree }

readP_degree :: ReadP Int
readP_degree = do { token "1" ; return 0 }
    +++ do { token "n" ; token "^"
           ; ds <- many1 $ satisfy isDigit ; skipSpaces
           ; return $ foldl ( \ n d -> 10*n + fromEnum d - fromEnum '0' ) 0 ds 
           }

token :: String -> ReadP ()
token s = do string s ; skipSpaces

parens :: ReadP a -> ReadP a
parens p = between (token "(") (token ")") p

instance Read Function where
    readsPrec _ = readP_to_S readP_Function

instance Show Function where
    show f = case f of
        Poly {} -> case degree f of
            Nothing -> "POLY"
            Just d -> case d of
                0 -> "O(1)"
                k -> "O(n^" ++ show k ++ ")"
        Finite -> "?" -- if this is upper bound, then the prefix is YES
        Infinite -> "?" -- if this is lower bound, then prefix is NO
