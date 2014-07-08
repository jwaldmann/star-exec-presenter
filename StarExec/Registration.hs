{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language DisambiguateRecordFields #-}

module StarExec.Registration where

import StarExec.Types ( Name )
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

full_categories mc = 
    filter ( \ c -> length (real_participants c) >= 2 ) $  categories mc

underfull_categories mc =
    filter ( \ c -> length (real_participants c) < 2 ) $  categories mc

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
    deriving ( Generic )

type Registration = Competition Catinfo

data Participant = 
     Participant { participantName :: Name
                 , solver_config :: Maybe (Int,Int) 
                 }
    deriving ( Generic )

standard n bs ps = Category {  categoryName = n , contents = 
    Catinfo { postproc = 163 , benchmarks = bs , participants = ps } }
certified n bs ps = Category { categoryName = n, contents = 
    Catinfo { postproc = 164 , benchmarks = bs , participants = ps } }

trss = [ All 12360 -- Der95
       , All 12347 -- AG01
       , All 12350 -- SK90
       ]

srss = [ All 12391 -- zan04
       , All 12383 -- Waldm07a
       , All 12392 -- Waldm07b
       , All 12393 -- ICFP 2010
       ]

mixed_rel_srs = All 12512
mixed_rel_trs = All 12381

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
      , standard "TRS Equational"  [ All 12399  ]
           [ Participant "AProVE" ( Just ( 1681, 2656  ) )
           , Participant "muterm" ( Just (1388, 2059))
           ]
      , standard "TRS Conditional"  [ All 12330 ]
           [ Participant "AProVE" ( Just ( 1681, 2656  ) )
           , Participant "muterm" ( Just (1388, 2059))
           ]
      , standard "TRS Context Sensitive"  [ All 12345 ]
           [ Participant "AProVE" ( Just ( 1681, 2656  ) )
           , Participant "muterm" ( Just (1388, 2059))
           ]
      , standard "TRS Innermost"  [ All 12341 ]
           [ Participant "AProVE" ( Just ( 1681, 2656  ) )
           , Participant "muterm" ( Just (1388, 2059))
           ]
      , standard "TRS Outermost"  [ All 12316 ]
           [ Participant "AProVE" ( Just ( 1681, 2656  ) )
           ]
      , certified "TRS Innermost certified"  [ All 12341 ]
           [ Participant "AProVE" ( Just ( 1681, 2652  ) )
           ]
      , certified "TRS Outermost certified"  [ All 12316 ]
           [ Participant "AProVE" ( Just ( 1681, 2652  ) )
           ]
      , standard "Higher-Order rewriting (union beta)"  [ All 12306 ]
           [ Participant "Wanda" ( Just (1542, 2390))
           , Participant "THOR" Nothing
           ]
     , standard "Integer Transition Systems"  [ All 51335 ]
           [ Participant "T2" ( Just ( 1739, 2751 ))
           , Participant "AProVE" ( Just ( 1681, 2658 ))
           , Participant "Ctrl" ( Just (1541, 2387))
           ]
     , standard "Integer TRS"  [ All 37558  ]
           [ Participant "AProVE" ( Just ( 1681, 2654  ) )
           , Participant "Ctrl" ( Just (1541, 2388))
           ]
     ]
   , MetaCategory "Complexity Analysis of Term Rewriting"
     [ standard "Derivational Complexity - Full Rewriting"  [ All 12473 ]
           [ Participant "TCT" ( Just (1620, 2518))
           , Participant "CaT" ( Just (1343, 1952))
           ]
     , standard "Runtime Complexity - Full Rewriting"  [ All 12402 ]
           [ Participant "TCT" ( Just (1620, 2516))
           , Participant "CaT" ( Just (1343, 1952))
           ]
     , standard "Runtime Complexity - Innermost Rewriting"  [ All 12258 ]
           [ Participant "TCT" ( Just (1620, 2515))
           , Participant "AProVE" ( Just ( 1681, 2656 ) )
           ]
     , certified "Derivational Complexity - Full Rewriting certified" [ All 12473 ]
           [ Participant "CaT" ( Just (1343, 1953))
           ]
     , certified "Runtime Complexity - Full Rewriting certified"   [ All 12402 ]
           [ Participant "CaT" ( Just (1343, 1953))
           ]
     , certified "Runtime Complexity - Innermost Rewriting certified"  [ All 12258 ]
           [ 
           ]
     ]
   , MetaCategory "Termination of Programming Languages"
     [ standard "C"  [ All 32448 ]
           [ Participant "AProVE" ( Just ( 1681,  2655 ) )
           , Participant "T2" ( Just ( 1739, 2751 ))
           , Participant "Ultimate Buchi Automizer" (Just (1730, 2738))
           , Participant "lsi.upc tool" Nothing
           ]
     , standard "Java"  [ All 12322 ]
           [ Participant "AProVE" ( Just ( 1681, 2657  ) )
           , Participant "Julia" Nothing
           ]
     , standard "Logic Programming"  [ All 12293 ]
           [ Participant "AProVE" ( Just ( 1681, 2653  ) )
           ]
     , standard "Functional Programming"  [ All 12254 ]
           [ Participant "AProVE" ( Just ( 1681, 2650  ) )
           ]
     ]
   ]



class Input t where input :: Parser t

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
        All { space = s } -> "All" <+> output s
        Hierarchy { space = s } -> "Hierarchy" <+> output s

p <#> q = fillBreak 4 p <+> q
showp = ( \ d -> displayS d "" ) . renderPretty 1.0 80 . output

instance Output a => Show ( Competition a) where show = showp
instance Output a => Show ( MetaCategory a) where show = showp
instance Output a => Show ( Category a ) where show = showp
instance Show Participant where show = showp
