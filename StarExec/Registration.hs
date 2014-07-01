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
    Catinfo { postproc = 141 , benchmarks = bs , participants = ps } }
certified n bs ps = Category { categoryName = n, contents = 
    Catinfo { postproc = 135 , benchmarks = bs , participants = ps } }

tc2014 :: Registration
tc2014 = Competition "Termination Competition 2014"
   [ MetaCategory "Termination of Term Rewriting (and Transition Systems)"
       [ standard "TRS Standard"  [ Bench 935202 ]
           [ Participant "TTT2" ( Just ( 1342, 1950 ))
           , Participant "NaTT" ( Just ( 1225, 1717))
           , Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "Wanda" ( Just (1542, 2389))
           , Participant "muterm" ( Just (1388, 2059))
           ]
       , standard "SRS Standard"  [ Bench 936511 ]
           [ Participant "TTT2" ( Just ( 1342, 1950 ))
           , Participant "NaTT" ( Just ( 1225, 1717))
           , Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "muterm" ( Just (1388, 2059))
           ]
       , standard "TRS Relative"  [ Bench 936234 ]
           [ Participant "TTT2" ( Just ( 1342, 1950 ))
           , Participant "AProVE" ( Just ( 1185, 1611  ) )
           ]
       , standard "SRS Relative"  [ Bench 943491 ]
           [ Participant "TTT2" ( Just ( 1342, 1950 ))
           , Participant "AProVE" ( Just ( 1185, 1611  ) )
           ]
      , certified "TRS Standard certified"  [ Bench 935202 ]
           [ Participant "TTT2"  ( Just ( 1342, 1951 ))
           , Participant "matchbox" ( Just ( 952, 1192 ))
           , Participant "AProVE" ( Just ( 1185, 1613  ) )
           ]
      , certified "SRS Standard certified"  [ Bench 936511 ]
           [ Participant "TTT2"  ( Just ( 1342, 1951 ))
           , Participant "matchbox"  ( Just ( 952, 1192 ))
           , Participant "AProVE" ( Just ( 1185, 1613  ) )
           ]
      , certified "TRS Relative certified"  [ Bench 936234 ]
           [ Participant "TTT2"  ( Just ( 1342, 1951 ))
           , Participant "AProVE" ( Just ( 1185, 1613  ) )
           ]
      , certified "SRS Relative certified"  [ Bench 943491 ]
           [ Participant "TTT2"  ( Just ( 1342, 1951 ))
           , Participant "AProVE" ( Just ( 1185, 1613  ) )
           ]
      , standard "TRS Equational"  [ Bench 937617 ]
           [ Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "muterm" ( Just (1388, 2059))
           ]
      , standard "TRS Conditional"  [ Bench 934159 ]
           [ Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "muterm" ( Just (1388, 2059))
           ]
      , standard "TRS Context Sensitive"  [ Bench 934738 ]
           [ Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "muterm" ( Just (1388, 2059))
           ]
      , standard "TRS Innermost"  [ Bench 934641 ]
           [ Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "muterm" ( Just (1388, 2059))
           ]
      , standard "TRS Outermost"  [ Bench 933838 ]
           [ Participant "AProVE" ( Just ( 1185, 1611  ) )
           ]
      , certified "TRS Innermost certified"  [ Bench 934641 ]
           [ Participant "AProVE" ( Just ( 1185, 1613  ) )
           ]
      , certified "TRS Outermost certified"  [ Bench 933838 ]
           [ Participant "AProVE" ( Just ( 1185, 1613  ) )
           ]
      , standard "Higher-Order rewriting (union beta)"  [ Bench 933481 ]
           [ Participant "Wanda" ( Just (1542, 2390))
           , Participant "THOR" Nothing
           ]
     , standard "Integer Transition Systems"  [ Bench 963614 ]
           [ Participant "T2" ( Just ( 1373, 2000 ))
           , Participant "AProVE" Nothing -- ( Just ( 1185,   ) )
           , Participant "Ctrl" ( Just (1541, 2387))
           ]
     , standard "Integer TRS"  [ Bench 956021 ]
           [ Participant "AProVE" Nothing -- ( Just ( 1185,   ) )
           , Participant "Ctrl" ( Just (1541, 2388))
           ]
     ]
   , MetaCategory "Complexity Analysis of Term Rewriting"
     [ standard "Derivational Complexity - Full Rewriting"  [ Bench 941674 ]
           [ Participant "TCT" ( Just (1382, 2025))
           , Participant "CaT" ( Just (1343, 1952))
           ]
     , standard "Runtime Complexity - Full Rewriting"  [ Bench 937691 ]
           [ Participant "TCT" ( Just (1382, 2023))
           , Participant "CaT" ( Just (1343, 1952))
           ]
     , standard "Runtime Complexity - Innermost Rewriting"  [ Bench 931609 ]
           [ Participant "TCT" ( Just (1382, 2022))
           , Participant "AProVE" ( Just ( 1185, 1611 ) )
           ]
     , certified "Derivational Complexity - Full Rewriting certified"  [ Bench 941674 ]
           [ Participant "CaT" ( Just (1343, 1953))
           ]
     , certified "Runtime Complexity - Full Rewriting certified"  [ Bench 937691 ]
           [ Participant "CaT" ( Just (1343, 1953))
           ]
     , certified "Runtime Complexity - Innermost Rewriting certified"  [ Bench 931609 ]
           [ 
           ]
     ]
   , MetaCategory "Termination of Programming Languages"
     [ standard "C"  [ Bench 964659 ]
           [ Participant "AProVE" ( Just ( 1185,  1608 ) )
           , Participant "T2" ( Just ( 1373, 2000 ))
           , Participant "Ultimate Buchi Automizer" (Just (1433, 2221))
           , Participant "lsi.upc tool" Nothing
           ]
     , standard "Java"  [ Bench 933941 ]
           [ Participant "AProVE" ( Just ( 1185, 1612  ) )
           , Participant "Julia" Nothing
           ]
     , standard "Logic Programming"  [ Bench 933167 ]
           [ Participant "AProVE" ( Just ( 1185, 1667  ) )
           ]
     , standard "Functional Programming"  [ Bench 930016 ]
           [ Participant "AProVE" ( Just ( 1185, 1606  ) )
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
