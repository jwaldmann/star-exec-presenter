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

data Competition = Competition { competitionName :: Name, metacategories :: [ MetaCategory ] }
    deriving ( Generic )
data MetaCategory = MetaCategory { metaCategoryName :: Name, categories :: [ Category ] }
    deriving ( Generic )
data Category = Category { categoryName :: Name , participants :: [ Participant ] }
    deriving ( Generic )
data Participant = Participant { participantName :: Name, solver_config :: Maybe (Int,Int) }
    deriving ( Generic )

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
instance Output Competition where
    output (Competition n mcs) = 
        ("Competition" <+> text (show n)) <#> output mcs
instance Output MetaCategory where 
    output (MetaCategory n cs) = 
        ("MetaCategory" <+> text (show n)) <#> output cs
instance Output Category where 
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

instance Show Competition where show = showp
instance Show MetaCategory where show = showp
instance Show Category where show = showp
instance Show Participant where show = showp

tc2014 :: Competition
tc2014 = Competition "Termination Competition 2014"
   [ MetaCategory "Termination of Term Rewriting (and Transition Systems)"
       [ Category "TRS Standard"
           [ Participant "TTT2" ( Just ( 1342, 1950 ))
           , Participant "NaTT" Nothing
           , Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "Wanda" Nothing
           , Participant "muterm" Nothing
           ]
       , Category "SRS Standard"
           [ Participant "TTT2" ( Just ( 1342, 1950 ))
           , Participant "NaTT" Nothing
           , Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "muterm" Nothing
           ]
       , Category "TRS Relative" 
           [ Participant "TTT2" ( Just ( 1342, 1950 ))
           , Participant "AProVE" ( Just ( 1185, 1611  ) )
           ]
       , Category "SRS Relative"
           [ Participant "TTT2" ( Just ( 1342, 1950 ))
           , Participant "AProVE" ( Just ( 1185, 1611  ) )
           ]
      , Category "TRS Standard certified"
           [ Participant "TTT2"  ( Just ( 1342, 1950 ))
           , Participant "matchbox" ( Just ( 952, 1192 ))
           , Participant "AProVE" ( Just ( 1185, 1613  ) )
           ]
      , Category "SRS Standard certified"
           [ Participant "TTT2"  ( Just ( 1342, 1950 ))
           , Participant "matchbox"  ( Just ( 952, 1192 ))
           , Participant "AProVE" ( Just ( 1185, 1613  ) )
           ]
      , Category "TRS Relative certified"
           [ Participant "TTT2"  ( Just ( 1342, 1950 ))
           , Participant "AProVE" ( Just ( 1185, 1613  ) )
           ]
      , Category "SRS Relative certified"
           [ Participant "TTT2"  ( Just ( 1342, 1950 ))
           , Participant "AProVE" ( Just ( 1185, 1613  ) )
           ]
      , Category "TRS Equational"
           [ Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "muterm" Nothing
           ]
      , Category "TRS Conditional"
           [ Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "muterm" Nothing
           ]
      , Category "TRS Context Sensitive"
           [ Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "muterm" Nothing
           ]
      , Category "TRS Innermost"
           [ Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "muterm" Nothing
           ]
      , Category "TRS Outermost"
           [ Participant "AProVE" ( Just ( 1185, 1611  ) )
           , Participant "muterm" Nothing
           ]
      , Category "TRS Innermost certified"
           [ Participant "AProVE" ( Just ( 1185, 1613  ) )
           ]
      , Category "TRS Outermost certified"
           [ Participant "AProVE" ( Just ( 1185, 1613  ) )
           ]
      , Category "Higher-Order rewriting (union beta)"
           [ Participant "Wanda" Nothing
           , Participant "THOR" Nothing
           ]
     , Category "Integer Transition Systems"
           [ Participant "T2" ( Just ( 1373, 2000 ))
           , Participant "AProVE" Nothing -- ( Just ( 1185,   ) )
           , Participant "Ctrl" Nothing
           ]
     , Category "Integer TRS"
           [ Participant "AProVE" Nothing -- ( Just ( 1185,   ) )
           , Participant "Ctrl" Nothing
           ]
     ]
   , MetaCategory "Complexity Analysis of Term Rewriting"
     [ Category "Derivational Complexity - Full Rewriting"
           [ Participant "TCT" ( Just (1382, 2025))
           , Participant "CaT" ( Just (1343, 1952))
           ]
     , Category "Runtime Complexity - Full Rewriting"
           [ Participant "TCT" ( Just (1382, 2023))
           , Participant "CaT" ( Just (1343, 1952))
           ]
     , Category "Runtime Complexity - Innermost Rewriting"
           [ Participant "TCT" ( Just (1382, 2022))
           , Participant "AProVE" ( Just ( 1185, 1611 ) )
           ]
     , Category "Derivational Complexity - Full Rewriting certified"
           [ Participant "CaT" ( Just (1343, 1953))
           ]
     , Category "Runtime Complexity - Full Rewriting certified"
           [ Participant "CaT" ( Just (1343, 1953))
           ]
     , Category "Runtime Complexity - Innermost Rewriting certified"
           [ Participant "AProVE" ( Just ( 1185, 1613 ) )
           ]
     ]
   , MetaCategory "Termination of Programming Languages"
     [ Category "C"
           [ Participant "AProVE" ( Just ( 1185,  1608 ) )
           , Participant "T2" ( Just ( 1373, 2000 ))
           , Participant "Ultimate Buchi Automizer" Nothing
           , Participant "lsi.upc tool" Nothing
           ]
     , Category "Java"
           [ Participant "AProVE" ( Just ( 1185, 1612  ) )
           , Participant "Julia" Nothing
           ]
     , Category "Logic Programming"
           [ Participant "AProVE" ( Just ( 1185, 1667  ) )
           ]
     , Category "Functional Programming"
           [ Participant "AProVE" ( Just ( 1185, 1606  ) )
           ]
     ]
   ]
