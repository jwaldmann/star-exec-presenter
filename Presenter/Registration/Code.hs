{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleInstances #-}

module Presenter.Registration.Code

( Registration (..)
, Competition (..)
, MetaCategory (..)
, Category (..)
, Catinfo (..)
, Benchmark_Source (..)
, Participant (..)

, full_categories
, unreal_categories
, all_categories
, demonstration_categories
, real_participants
, hors_concours
, participant_names

, parts, filterP, prune, insert, fill

)

where

import Presenter.Model ( Name)

import qualified Data.Text as T
import qualified Data.Set as S

import Prelude

import GHC.Generics

import Presenter.Model.RouteTypes
import Presenter.Output

import Text.PrettyPrint.Leijen as P hiding ((<$>), fill)
import Data.List ( intersperse )
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token as T
import Text.Parsec.Language (haskell)
import Control.Monad ( zipWithM )

-- * data types

data Competition a =
     Competition { competitionName :: Name
                 , metacategories :: [ MetaCategory a ]
                 }
    deriving ( Eq, Generic )

instance Functor Competition where
    fmap f c = c { metacategories = map (fmap f) $ metacategories c }

data MetaCategory a =
     MetaCategory { metaCategoryName :: Name
                  , categories :: [ Category a ]
                  }
    deriving ( Eq, Generic )

all_categories :: MetaCategory Catinfo -> [Category Catinfo]
all_categories mc =
    filter ( \ c -> length (real_participants c) >= 1 ) $  categories mc

-- | categories that have at least two real participants
full_categories :: MetaCategory Catinfo -> [Category Catinfo]
full_categories mc =
    filter ( \ c -> length (real_participants c) >= 2 ) $  categories mc

-- | categories that have one real participant
demonstration_categories :: MetaCategory Catinfo -> [Category Catinfo]
demonstration_categories mc =
    filter ( \ c -> length (real_participants c) == 1 ) $  categories mc

-- | categories that have no real participants
unreal_categories :: MetaCategory Catinfo -> [Category Catinfo]
unreal_categories mc =
    filter ( \ c -> length (real_participants c) == 0 ) $  categories mc

-- |  a real participant has a config
real_participants :: Category Catinfo -> [Participant]
real_participants c
  = filter ( \ p -> case solver_config p of
                Just (_,so,_) -> not $ hoco so
                Nothing -> False )
  $ participants
  $ contents c

-- HACK, FIXME (must be configurable)
hors_concours :: SolverID -> Bool
hors_concours (StarExecSolverID id) = hoco id

hoco id = id == 3797

instance Functor MetaCategory where
    fmap f c = c { categories = map (fmap f) $ categories c }

data Category a =
     Category { categoryName :: Name
              , contents :: a
              }
    deriving ( Eq, Generic )

instance Functor Category where
    fmap f c = c { contents = f $ contents c }

data Catinfo =
     Catinfo { postproc :: Int
             , benchmarks :: [ Benchmark_Source ]
             , participants :: [ Participant ]
             }
    deriving ( Eq, Generic )

data Benchmark_Source
  = Bench { bench :: Int }
  | All { space :: Int }
  | Hierarchy { space :: Int }
    deriving ( Eq, Generic, Show )

type Registration = Competition Catinfo

data Participant =
     Participant { participantName :: Name
                 , solver_config :: Maybe (Int,Int,Int) -- ^ space,solver,config
                 }
    deriving ( Eq, Ord, Generic )


-- * tree manipulation (tool set)

class Parts a where
  parts :: a -> S.Set Participant
instance Parts Catinfo where
  parts = S.fromList . participants
instance Parts a => Parts (Category a) where
  parts = parts . contents
instance Parts a => Parts (MetaCategory a) where
  parts mc = S.unions $ map parts $ categories mc
instance Parts a => Parts (Competition a) where
  parts c = S.unions $ map parts $ metacategories c

participant_names c =
  S.toList $ S.map participantName $ parts c

class FilterP a where
  filterP :: (Participant -> Bool) -> a -> a
instance FilterP Catinfo where
  filterP p c = c { participants = filter p $ participants c }
instance FilterP a => FilterP (Category a) where
  filterP p c = c { contents = filterP p $ contents c }
instance FilterP a => FilterP (MetaCategory a) where
  filterP p c = c { categories = map (filterP p) $ categories c }
instance FilterP a => FilterP (Competition a) where
  filterP p c = c { metacategories = map (filterP p) $ metacategories c }


-- | remove nodes with empty list of children
class Prune a where prune :: a -> a
instance Prune (MetaCategory [a]) where
  prune mc = mc { categories = filter (not.null.contents)
                               $ categories mc }
instance Prune (Competition [a]) where
  prune c = c { metacategories = filter (not.null.categories)
                               $ map prune $ metacategories c }

-- | right argument can add information in the leaves
-- (new participants) but must keep information
-- in the inner nodes (names of category, etc.).
-- may omit some inner nodes. cannot invent inner nodes.
-- (need to start it with a skeletal tree
-- (only inner nodes, leaves with empty participant lists)
class Insert a where
  insert :: [a] -> [a] -> Either String [a]

instance Insert Participant where
  insert xs ys = do
    let int = S.intersection
          (S.fromList $ map participantName xs)
          (S.fromList $ map participantName ys)
    if not $ S.null int
      then Left $ unlines
          [ unwords [ "insert", show xs, show ys ]
          , "forbidden because of overlaps:"
          , show int
          ]
      else return $ xs ++ ys

inst :: (Eq name, Insert a, Show b)
     => String
     -> (name -> [a] -> b) -> (b->name) -> (b -> [a])
     -> [b] -> [b] -> Either String [b]
inst tag cons name cont =
  let go xs [] = Right xs
      go [] ys = Left $ unlines
        [ "forbidden " ++ tag ++ "(extra names or wrong order)"
        , show ys
        ]
      go (x:xs)(y:ys) =
        if name x == name y
        then do
          xy <- cons (name x) <$> insert (cont x) (cont y)
          (xy :) <$> go xs ys
        else (x:) <$> go xs (y:ys)
  in  go

instance (Output a, Insert a) => Insert (Category [a]) where
  insert = inst "categories" Category categoryName contents

instance (Output a, Insert a) => Insert (MetaCategory [a]) where
  insert = inst "metacategories"
           MetaCategory metaCategoryName categories

instance (Output a, Insert a) => Insert (Competition [a]) where
  insert = inst "competition"
     Competition competitionName metacategories

-- | put (participant) information from b into a
class Fill a b where
  fill :: a -> b -> Either String a

assertEq x y =
  if x == y
  then Right ()
  else Left $ unwords [ "must be equal:", show x, show y ]

instance Fill Catinfo [Participant] where
  fill ci ps = return $ ci { participants = ps }
instance Fill (Category Catinfo) (Category [Participant]) where
  fill c d = do
    assertEq (categoryName c) (categoryName d)
    o <- fill (contents c) (contents d)
    return $ c { contents = o }
instance Fill (MetaCategory Catinfo) (MetaCategory [Participant]) where
  fill c d = do
    assertEq (metaCategoryName c) (metaCategoryName d)
    o <- zipWithM fill (categories c) (categories d)
    return $ c { categories = o }
instance Fill (Competition Catinfo) (Competition [Participant]) where
  fill c d = do
    assertEq (competitionName c) (competitionName d)
    o <- zipWithM fill (metacategories c) (metacategories d)
    return $ c { metacategories = o }

-- * I/O formatting

class Input t where input :: Parser t

lexer :: TokenParser st
lexer = haskell

instance Input Int where input = fromIntegral <$> T.integer lexer
instance Input a => Input [a] where
    input = T.brackets haskell $ commaSep lexer input
instance Input a => Input (Maybe a) where
    input = do reserved lexer "Nothing" ; return Nothing
        <|> do reserved lexer "Just" ; x <- input ; return $ Just x
instance (Input a, Input b) => Input (a,b) where
    input = T.parens lexer $ do x <- input ; _ <- T.comma lexer ; y <- input ; return (x,y)
instance Input T.Text where
    input = T.pack <$> T.stringLiteral lexer
instance Input Participant where
    input = do
        T.reserved lexer "Participant"
        T.braces lexer undefined

instance Output a => Output (Competition a) where
    output (Competition n mcs) =
        ("Competition" <+> text (show n)) <#> output mcs
instance Output a => Output (MetaCategory a) where
    output (MetaCategory n cs) =
        ("MetaCategory" <+> text (show n)) <#> output cs
instance Output a => Output (Category a) where
    output (Category n ps) =
        ("Category" <+> text (show n)) <#> output ps
instance Output Participant where
    output p =
        "Participant" <+> P.braces ( hsep $ intersperse ","
             [ "participantName" <+> equals <+> output (participantName p)
             , "solver_config" <+> equals <+> output (solver_config p)
             ] )
instance Output Catinfo where
    output i = "Catinfo" <+> P.braces ( hsep $ intersperse ","
             [ "postproc" <+> equals <+> output (postproc i)
             , "benchmarks" <+>  equals <+> output (benchmarks i)
             , "participants" <+> equals <+> output ( participants i)
             ] )
instance Output Benchmark_Source where
    output s = case s of
        Bench { bench = i } -> "Bench" <+> output i
        All { space = s' } -> "All" <+> output s'
        Hierarchy { space = s' } -> "Hierarchy" <+> output s'

instance Output a => Show ( Competition a) where show = showp
instance Output a => Show ( MetaCategory a) where show = showp
instance Output a => Show ( Category a ) where show = showp
instance Show Participant where show = showp
