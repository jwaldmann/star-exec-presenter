{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language DisambiguateRecordFields #-}

module Presenter.Registration 
( the_competition
, full_categories
, demonstration_categories
, real_participants
, Competition (..)
, MetaCategory (..)
, Category (..)
, Catinfo (..)
, Benchmark_Source (..)
, Participant (..)
) where

import Presenter.Model ( Name )
import qualified Data.Text as T

import Prelude 

import GHC.Generics

import Text.PrettyPrint.Leijen as P hiding ((<$>)) 
import Data.String
import Data.List ( intersperse )
import Text.Parsec 
import Text.Parsec.String
import Text.Parsec.Token as T
import Text.Parsec.Language (haskell)
import Control.Applicative ( (<$> ))
import Data.Maybe ( isJust )

data Competition a = 
     Competition { competitionName :: Name
                 , metacategories :: [ MetaCategory a ] 
                 }
    deriving ( Generic )

instance Functor Competition where 
    fmap f c = c { metacategories = map (fmap f) $ metacategories c }

data MetaCategory a = 
     MetaCategory { metaCategoryName :: Name
                  , categories :: [ Category a ] 
                  }
    deriving ( Generic )

full_categories :: MetaCategory Catinfo -> [Category Catinfo]
full_categories mc = 
    filter ( \ c -> length (real_participants c) >= 2 ) $  categories mc

demonstration_categories :: MetaCategory Catinfo -> [Category Catinfo]
demonstration_categories mc =
    filter ( \ c -> length (real_participants c) == 1 ) $  categories mc

real_participants :: Category Catinfo -> [Participant]
real_participants c = 
    filter ( isJust . solver_config ) $ participants $ contents c

instance Functor MetaCategory where 
    fmap f c = c { categories = map (fmap f) $ categories c }


data Category a = 
     Category { categoryName :: Name 
              , contents :: a 
              }
    deriving ( Generic )

instance Functor Category where 
    fmap f c = c { contents = f $ contents c }

data Catinfo = 
     Catinfo { postproc :: Int
             , benchmarks :: [ Benchmark_Source ]
             , participants :: [ Participant ]
             }
    deriving ( Generic )

data Benchmark_Source =
       Bench { bench :: Int } | All { space :: Int } | Hierarchy { space :: Int }
    deriving ( Generic, Show )

type Registration = Competition Catinfo

data Participant = 
     Participant { participantName :: Name
                 , solver_config :: Maybe (Int,Int) 
                 }
    deriving ( Generic )

standard :: Name -> [Benchmark_Source] -> [Participant] -> Category Catinfo
standard n bs ps = Category {  categoryName = n , contents = 
    Catinfo { postproc = 163 , benchmarks = bs , participants = ps } }
certified :: Name -> [Benchmark_Source] -> [Participant] -> Category Catinfo
certified n bs ps = Category { categoryName = n, contents = 
    Catinfo { postproc = 172 , benchmarks = bs , participants = ps } }

trss :: [Benchmark_Source]
trss = [ Hierarchy 56849 ]

srss :: [Benchmark_Source]
srss = [ Hierarchy 56810 ]

mixed_rel_srs :: Benchmark_Source
mixed_rel_srs = Hierarchy 56805

mixed_rel_trs :: Benchmark_Source
mixed_rel_trs = Hierarchy 56846

the_competition = experiment2015

experiment2015 :: Registration
experiment2015 = Competition "Experiments for 2015"
   [ MetaCategory "Complexity Analysis of Term Rewriting"
     [ standard "Derivational Complexity - Full Rewriting"  [ Hierarchy 56613 ]
           [ -- Participant "matchbox-complex-boolector" ( Just ( 2536, 17921 ))
           -- , Participant "matchbox-complex-satchmo" ( Just ( 2536, 17912 ))             
             Participant "matchbox-complex-satchmo-repaired" ( Just ( 2649, 19511 ))             
             --  Participant "matchbox-nocon-complex-boolector" ( Just ( 2536, 17918 )) 
             -- , Participant "matchbox-nocon-complex-satchmo" ( Just ( 2536, 17919 ))
             , Participant "matchbox-nocon-complex-satchmo-repaired" ( Just ( 2649, 19518 )) 
           ]
     ]
   , MetaCategory "Termination of Term Rewriting (and Transition Systems)"
       [ -- standard "TRS Standard"  trss maparts_std
         standard "SRS Standard"  srss maparts_std
       -- , certified "TRS Standard certified"  trss maparts_cert
       , certified "SRS Standard certified"  srss maparts_cert
       ]
   ]

maparts_std = 
  [ -- Participant "matchbox-dp-boolector" ( Just ( 2536, 17916 ))
  -- , Participant "matchbox-dp-satchmo" ( Just ( 2536, 17913 ))
    Participant "matchbox-dp-satchmo-repaired" ( Just ( 2649, 19512 ))
    --  Participant "matchbox-dp-ur-boolector" ( Just ( 2536, 17911 ))
    -- , Participant "matchbox-dp-ur-satchmo" ( Just ( 2536, 17920 ))
    , Participant "matchbox-dp-ur-satchmo-repaired" ( Just ( 2649, 19519 ))
  ]

maparts_cert = 
  [ -- Participant "matchbox-nocon-dp-boolector" ( Just ( 2536, 17910 ))
    -- , Participant "matchbox-nocon-dp-satchmo" ( Just ( 2536, 17914 ))
      Participant "matchbox-nocon-dp-satchmo-repaired" ( Just ( 2649, 19513 ))
    -- , Participant "matchbox-nocon-dp-ur-boolector" ( Just ( 2536, 17917 ))
    -- , Participant "matchbox-nocon-dp-ur-satchmo" ( Just ( 2536, 17915 ))
    , Participant "matchbox-nocon-dp-ur-satchmo-repaired" ( Just ( 2649, 19514 ))
  ]

tc2014 :: Registration
tc2014 = Competition "Termination Competition 2014"
   [ MetaCategory "Termination of Term Rewriting (and Transition Systems)"
       [ standard "TRS Standard"  trss
           [ Participant "TTT2" ( Just ( 1342, 1950 ))
           , Participant "NaTT" ( Just ( 1225, 2514))
           , Participant "AProVE" ( Just ( 1681, 2656 ) )
           , Participant "Wanda" ( Just (1542, 2389))
           , Participant "muterm" ( Just (1388, 2059))
           -- , Participant "matchbox" ( Just ( 1790, 2847 ))
           ]
       , standard "SRS Standard"  srss
           [ Participant "TTT2" ( Just ( 1342, 1950 ))
           , Participant "NaTT" ( Just ( 1225, 2514))
           , Participant "AProVE" ( Just ( 1681, 2656  ) )
           , Participant "muterm" ( Just (1388, 2059))
           -- , Participant "matchbox" ( Just ( 1790, 2847 ))
           ]
       , standard "TRS Relative"  [ mixed_rel_trs ]
           [ Participant "TTT2" ( Just ( 1342, 1950 ))
           , Participant "AProVE" ( Just ( 1681, 2656  ) )
           ]
       , standard "SRS Relative"  [ mixed_rel_srs ]
           [ Participant "TTT2" ( Just ( 1342, 1950 ))
           , Participant "AProVE" ( Just ( 1681, 2656  ) )
           ]
      , certified "TRS Standard certified"  trss
           [ Participant "TTT2"  ( Just ( 1342, 1951 ))
           , Participant "matchbox" ( Just ( 1790, 2846 ))
           , Participant "AProVE" ( Just ( 1681, 2652  ) )
           ]
      , certified "SRS Standard certified"  srss
           [ Participant "TTT2"  ( Just ( 1342, 1951 ))
           , Participant "matchbox"  ( Just ( 1790, 2846 ))
           , Participant "AProVE" ( Just ( 1681, 2652  ) )
           ]
      , certified "TRS Relative certified"  [ mixed_rel_trs ]
           [ Participant "TTT2"  ( Just ( 1342, 1951 ))
           , Participant "AProVE" ( Just ( 1681, 2652  ) )
           ]
      , certified "SRS Relative certified"  [ mixed_rel_srs ]
           [ Participant "TTT2"  ( Just ( 1342, 1951 ))
           , Participant "AProVE" ( Just ( 1681, 2652  ) )
           ]
      , standard "TRS Equational"  [ Hierarchy 56831  ]
           [ Participant "AProVE" ( Just ( 1681, 2656  ) )
           , Participant "muterm" ( Just (1388, 2059))
           ]
      , standard "TRS Conditional"  [ Hierarchy 56824 ]
           [ Participant "AProVE" ( Just ( 1681, 2656  ) )
           , Participant "muterm" ( Just (1388, 2059))
           ]
      , standard "TRS Context Sensitive"  [ Hierarchy 56827 ]
           [ Participant "AProVE" ( Just ( 1681, 2656  ) )
           , Participant "muterm" ( Just (1388, 2059))
           ]
      , standard "TRS Innermost"  [ Hierarchy 56836 ]
           [ Participant "AProVE" ( Just ( 1681, 2656  ) )
           , Participant "muterm" ( Just (1388, 2059))
           ]
      , standard "TRS Outermost"  [ Hierarchy 56842 ]
           [ Participant "AProVE" ( Just ( 1681, 2656  ) )
           ]
      , certified "TRS Innermost certified"  [ Hierarchy 56836 ]
           [ Participant "AProVE" ( Just ( 1681, 2652  ) )
           ]
      , certified "TRS Outermost certified"  [ Hierarchy 56842  ]
           [ Participant "AProVE" ( Just ( 1681, 2652  ) )
           ]
      , standard "Higher-Order rewriting (union beta)"  
           [ Hierarchy 56698 ]
           [ Participant "Wanda" ( Just (1542, 2390))
           , Participant "THOR" ( Just (1800, 2862))
           ]
     , standard "Integer Transition Systems"  [ Hierarchy 56706 ]
           [ Participant "T2" ( Just ( 1739, 2751 ))
           , Participant "AProVE" ( Just ( 1681, 2894 ))
           , Participant "Ctrl" ( Just (1541, 2387))
           , Participant "CppInv" ( Just (1803, 2870))
           ]
     , standard "Integer TRS"  [ Hierarchy 56704  ]
           [ Participant "AProVE" ( Just ( 1681, 2654  ) )
           , Participant "Ctrl" ( Just (1541, 2388))
           ]
     ]
   , MetaCategory "Complexity Analysis of Term Rewriting"
     [ standard "Derivational Complexity - Full Rewriting"  [ Hierarchy 56613 ]
           [ Participant "TCT" ( Just (1620, 2908))
           , Participant "CaT" ( Just (1343, 1952))
           ]
     , standard "Runtime Complexity - Full Rewriting"  [ Hierarchy 56748 ]
           [ Participant "TCT" ( Just (1620, 2909))
           , Participant "CaT" ( Just (1343, 1952))
           ]
     , standard "Runtime Complexity - Innermost Rewriting"  [ Hierarchy 56775 ]
           [ Participant "TCT" ( Just (1620, 2910))
           , Participant "AProVE" ( Just ( 1681, 2656 ) )
           ]
     , certified "Derivational Complexity - Full Rewriting certified" [ Hierarchy 56613 ]
           [ Participant "CaT" ( Just (1343, 1953))
           ]
     , certified "Runtime Complexity - Full Rewriting certified"   [ Hierarchy 56748 ]
           [ Participant "CaT" ( Just (1343, 1953))
           ]
     , certified "Runtime Complexity - Innermost Rewriting certified"  [ Hierarchy 56775 ]
           [ 
           ]
     ]
   , MetaCategory "Termination of Programming Languages"
     [ standard "C"  [ Hierarchy 56607 ]
           [ Participant "AProVE" ( Just ( 1681,  2655 ) )
           , Participant "T2" ( Just ( 1739, 2751 ))
           , Participant "Ultimate Buchi Automizer" (Just (1730, 2738))
           -- , Participant "lsi.upc tool" Nothing
           ]
     , standard "Java"  [ Hierarchy 56709, Hierarchy 56721 ]
           [ Participant "AProVE" ( Just ( 1681, 2657  ) )
           -- , Participant "Julia" Nothing
           ]
     , standard "Logic Programming"  [ Hierarchy 56728, Hierarchy 56739, Hierarchy 56744 ]
           [ Participant "AProVE" ( Just ( 1681, 2653  ) )
           ]
     , standard "Functional Programming"  [ Hierarchy 56695 ]
           [ Participant "AProVE" ( Just ( 1681, 2650  ) )
           ]
     ]
   ]



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
    input = T.parens lexer $ do x <- input ; T.comma lexer ; y <- input ; return (x,y)
instance Input T.Text where
    input = T.pack <$> T.stringLiteral lexer
instance Input Participant where
    input = do 
        T.reserved lexer "Participant"
        T.braces lexer $ undefined

class Output t where output :: t -> Doc
instance IsString Doc where fromString = text

instance Output Int where 
    output = text . show
instance Output T.Text where
    output = text . show
instance Output t => Output [t] where 
    output = list . map output
instance Output t => Output (Maybe t) where
    output x = case x of
        Nothing -> "Nothing"
        Just a -> "Just" <+> align (output a)
instance (Output a, Output b) => Output (a,b) where
    output (x,y) = tupled [ output x, output y ]
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
             [ "name" <+> equals <+> output (participantName p)
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

(<#>) :: Doc -> Doc -> Doc
p <#> q = fillBreak 4 p <+> q

showp :: Output a => a -> String
showp = ( \ d -> displayS d "" ) . renderPretty 1.0 80 . output

instance Output a => Show ( Competition a) where show = showp
instance Output a => Show ( MetaCategory a) where show = showp
instance Output a => Show ( Category a ) where show = showp
instance Show Participant where show = showp
