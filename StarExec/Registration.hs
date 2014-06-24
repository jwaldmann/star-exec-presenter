{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}

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

standard bs ps = Catinfo { postproc = 44 , benchmarks = bs , participants = ps }
certified bs ps = Catinfo { postproc = 130 , benchmarks = bs , participants = ps }

tc2014 :: Registration
tc2014 = Competition "Termination Competition 2014"
   [ MetaCategory "Termination of Term Rewriting (and Transition Systems)"
       [ Category "TRS Standard" $ standard [ Bench 935202 ]
           [ Participant "TTT2" ( Just ( 1342, 1950 ))
           , Participant "NaTT" Nothing
           , Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "Wanda" Nothing
           , Participant "muterm" ( Just (1388, 2059))
           ]
       , Category "SRS Standard" $ standard [ Bench 936511 ]
           [ Participant "TTT2" ( Just ( 1342, 1950 ))
           , Participant "NaTT" Nothing
           , Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "muterm" ( Just (1388, 2059))
           ]
       , Category "TRS Relative" $ standard [ Bench 936234 ]
           [ Participant "TTT2" ( Just ( 1342, 1950 ))
           , Participant "AProVE" ( Just ( 1185, 1611  ) )
           ]
       , Category "SRS Relative" $ standard [ Bench 943491 ]
           [ Participant "TTT2" ( Just ( 1342, 1950 ))
           , Participant "AProVE" ( Just ( 1185, 1611  ) )
           ]
      , Category "TRS Standard certified" $ certified [ Bench 935202 ]
           [ Participant "TTT2"  ( Just ( 1342, 1950 ))
           , Participant "matchbox" ( Just ( 952, 1192 ))
           , Participant "AProVE" ( Just ( 1185, 1613  ) )
           ]
      , Category "SRS Standard certified" $ certified [ Bench 936511 ]
           [ Participant "TTT2"  ( Just ( 1342, 1950 ))
           , Participant "matchbox"  ( Just ( 952, 1192 ))
           , Participant "AProVE" ( Just ( 1185, 1613  ) )
           ]
      , Category "TRS Relative certified" $ certified [ Bench 936234 ]
           [ Participant "TTT2"  ( Just ( 1342, 1950 ))
           , Participant "AProVE" ( Just ( 1185, 1613  ) )
           ]
      , Category "SRS Relative certified" $ certified [ Bench 943491 ]
           [ Participant "TTT2"  ( Just ( 1342, 1950 ))
           , Participant "AProVE" ( Just ( 1185, 1613  ) )
           ]
      , Category "TRS Equational" $ standard [ Bench 937617 ]
           [ Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "muterm" ( Just (1388, 2059))
           ]
      , Category "TRS Conditional" $ standard [ Bench 934159 ]
           [ Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "muterm" ( Just (1388, 2059))
           ]
      , Category "TRS Context Sensitive" $ standard [ Bench 934738 ]
           [ Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "muterm" ( Just (1388, 2059))
           ]
      , Category "TRS Innermost" $ standard [ Bench 934641 ]
           [ Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "muterm" ( Just (1388, 2059))
           ]
      , Category "TRS Outermost" $ standard [ Bench 933838 ]
           [ Participant "AProVE" ( Just ( 1185, 1611  ) )
           ]
      , Category "TRS Innermost certified" $ certified [ Bench 934641 ]
           [ Participant "AProVE" ( Just ( 1185, 1613  ) )
           ]
      , Category "TRS Outermost certified" $ certified [ Bench 933838 ]
           [ Participant "AProVE" ( Just ( 1185, 1613  ) )
           ]
      , Category "Higher-Order rewriting (union beta)" $ standard [ Bench 933481 ]
           [ Participant "Wanda" Nothing
           , Participant "THOR" Nothing
           ]
     , Category "Integer Transition Systems" $ standard [ Bench 964032 ]
           [ Participant "T2" ( Just ( 1373, 2000 ))
           , Participant "AProVE" Nothing -- ( Just ( 1185,   ) )
           , Participant "Ctrl" Nothing
           ]
     , Category "Integer TRS" $ standard []
           [ Participant "AProVE" Nothing -- ( Just ( 1185,   ) )
           , Participant "Ctrl" Nothing
           ]
     ]
   , MetaCategory "Complexity Analysis of Term Rewriting"
     [ Category "Derivational Complexity - Full Rewriting" $ standard [ Bench 941674 ]
           [ Participant "TCT" ( Just (1382, 2025))
           , Participant "CaT" ( Just (1343, 1952))
           ]
     , Category "Runtime Complexity - Full Rewriting" $ standard [ Bench 937691 ]
           [ Participant "TCT" ( Just (1382, 2023))
           , Participant "CaT" ( Just (1343, 1952))
           ]
     , Category "Runtime Complexity - Innermost Rewriting" $ standard [ Bench 931609 ]
           [ Participant "TCT" ( Just (1382, 2022))
           , Participant "AProVE" ( Just ( 1185, 1611 ) )
           ]
     , Category "Derivational Complexity - Full Rewriting certified" $ certified [ Bench 941674 ]
           [ Participant "CaT" ( Just (1343, 1953))
           ]
     , Category "Runtime Complexity - Full Rewriting certified" $ certified [ Bench 937691 ]
           [ Participant "CaT" ( Just (1343, 1953))
           ]
     , Category "Runtime Complexity - Innermost Rewriting certified" $ certified [ Bench 931609 ]
           [ Participant "AProVE" ( Just ( 1185, 1613 ) )
           ]
     ]
   , MetaCategory "Termination of Programming Languages"
     [ Category "C" $ standard [ Bench 964659 ]
           [ Participant "AProVE" ( Just ( 1185,  1608 ) )
           , Participant "T2" ( Just ( 1373, 2000 ))
           , Participant "Ultimate Buchi Automizer" Nothing
           , Participant "lsi.upc tool" Nothing
           ]
     , Category "Java" $ standard [ Bench 933941 ]
           [ Participant "AProVE" ( Just ( 1185, 1612  ) )
           , Participant "Julia" Nothing
           ]
     , Category "Logic Programming" $ standard [ Bench 933167 ]
           [ Participant "AProVE" ( Just ( 1185, 1667  ) )
           ]
     , Category "Functional Programming" $ standard [ Bench 930016 ]
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

p <#> q = fillBreak 4 p <+> q
showp = ( \ d -> displayS d "" ) . renderPretty 1.0 80 . output

instance Output a => Show ( Competition a) where show = showp
instance Output a => Show ( MetaCategory a) where show = showp
instance Output a => Show ( Category a ) where show = showp
instance Show Participant where show = showp
