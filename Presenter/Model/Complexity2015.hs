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

-- | NOTE: Ord instance is required for keys in maps (?)
-- but otherwise useless
data Bounds = Bounds { lower :: Function
                     , upper :: Function
                     }
     deriving (Eq, Ord)

-- | test all branches of the original grammar:
b1 :: Bounds
b1 = Bounds { lower = Infinite, upper = Infinite } -- MAYBE
b2 :: Bounds
b2 = Bounds { lower = Finite, upper = Infinite } -- NON_POLY lower bound
b3 :: Bounds
b3 = Bounds { lower = Poly $ Just 2, upper = Finite } -- termination is known
b4 :: Bounds
b4 = Bounds { lower = Poly $ Just 2, upper = Infinite } -- termination is not known
b5 :: Bounds
b5 = Bounds { lower = Poly $ Just 1, upper = Poly $ Just 3 }
b6 :: Bounds
b6 = Bounds { lower = Finite , upper = Poly Nothing }
b7 :: Bounds
b7 = Bounds { lower = Expo, upper = Infinite }

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
          Expo -> "e"
          Finite -> "?" ; Infinite -> "-"
        ssupper f = case f of
          Poly Nothing -> "n^?" ; Poly (Just d) -> "n^" ++ show d
          Expo -> "e"
          Finite -> "-" ; Infinite -> "?"
    in  T.pack $ sslower (lower b) ++ "/" ++ ssupper (upper b)

instance Read Bounds where
    readsPrec _ = readP_to_S $ skipSpaces *> readP_Bounds

readP_Bounds :: ReadP Bounds
readP_Bounds = parens readP_Bounds_bare +++ readP_Bounds_bare

-- | NOTE: the parser assumes the trivial lower bound (Finite)
-- and the trivial upper bound (Infinite) and refines this
-- if it reads a statement other than "?" for the respective bound.
readP_Bounds_bare :: ReadP Bounds
readP_Bounds_bare =
        do { token "WORST_CASE" ; pair Finite Infinite }
    +++ do { token "MAYBE" ; pair Finite Infinite }

pair :: Function -> Function -> ReadP Bounds
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
              | Expo -- ^ unknown exponential
              | Finite -- ^ some unspecified function (each value is finite)
              | Infinite -- ^ some function that has infinite values somewhere
     deriving (Eq, Ord)

-- * comparison of functions for bounds.
-- need to know the context (is it for upper or lower bounds).
-- lower bounds, from bad to good:
-- Finite = Poly Nothing = Poly (Just 0) < Poly (Just 1) < .. < Expo < Infinite
-- where Infinite means nontermination.
-- upper bounds, from good to bad (NOTE the order)
-- Poly (Just 0) < Poly (Just 1) < .. < Poly Nothing < Expo < Finite < Infinite
-- these orders are computed by the following comparison functions.


-- | @compare_for_lower_bounds f g@ says GT iff f is larger than g
-- in the sense that  Omega(f) is a subset of Omega (g).
-- The type is actually wrong (it assumes that the order is total).
compare_for_lower_bounds :: Function -> Function -> Ordering
compare_for_lower_bounds f g = case (f,g) of
  ( _ , _ ) | f == g -> EQ
  ( Infinite, _ ) -> GT
  ( _ , Infinite ) -> LT
  ( Expo , _ ) -> GT
  ( _ , Expo ) -> LT
  ( Poly (Just p), Poly (Just q)) -> compare p q
  ( Poly (Just p), _ ) | p > 0 -> GT
  ( _, Poly (Just q) ) | q > 0 -> LT
  -- here, we have Finite or Poly Nothing or Poly (Just 0)
  ( _, _ ) -> EQ

-- | @compare_for_upper_bounds f g@ says GT iff f is larger than g
-- in the sense that  Oh(f) is a superset of Oh (g).
-- The type is actually wrong (it assumes that the order is total).
compare_for_upper_bounds :: Function -> Function -> Ordering
compare_for_upper_bounds f g = case (f,g) of
  ( _ , _ ) | f == g -> EQ
  ( Infinite, _ ) -> GT
  ( _ , Infinite ) -> LT
  ( Finite, _ ) -> GT
  ( _ , Finite ) -> LT
  ( Expo, _ ) -> GT
  ( _, Expo ) -> LT
  ( Poly Nothing, Poly (Just _) ) -> GT
  ( Poly (Just _), Poly Nothing ) -> LT
  ( Poly (Just p), Poly (Just q)) -> compare p q

isPoly :: Function -> Bool
isPoly f = case f of Poly {} -> True ; _ -> False

readP_FunctionL :: ReadP Function
readP_FunctionL = do { token "NON_POLY" ; return Expo }
    +++ do { token "Omega" ; parens $ ( Poly . Just ) <$> readP_degreeL }

readP_degreeL :: ReadP Int
readP_degreeL =
  do { token "n" ; token "^"
     ; ds <- many1 $ satisfy isDigit ; skipSpaces
     ; return $ foldl ( \ n d -> 10*n + fromEnum d - fromEnum '0' ) 0 ds
     }

readP_FunctionU :: ReadP Function
readP_FunctionU = do { token "POLY" ; return $ Poly $ Nothing }
    +++ do { token "O" ; parens $ ( Poly . Just ) <$> readP_degreeU }

readP_degreeU :: ReadP Int
readP_degreeU = do { token "1" ; return 0 }
    +++ readP_degreeL

token :: String -> ReadP ()
token s = do
  _ <- string s
  skipSpaces

parens = between (token "(") (token ")")

showLower :: Function -> String
showLower f = case f of
  Poly {} -> case degree f of
    Nothing -> "POLY"
    Just d -> "Omega(n^" ++ show d ++ ")"
  Expo -> "NON_POLY"
  Finite -> "?"
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
