{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleInstances #-}
{-# language StandaloneDeriving #-}

module Presenter.Registration.Data

( -- extract, bare, code, skeleton,
  collect
, experiment2015, tc2014
)
       
where

import Presenter.Model ( Name, Year (..) )
import Presenter.Registration.Code

import qualified Data.Text as T
import qualified Data.Set as S

import Prelude 

import GHC.Generics

import Text.PrettyPrint.Leijen as P hiding ((<$>), fill) 
import Data.String
import Data.List ( intersperse )
import Text.Parsec 
import Text.Parsec.String
import Text.Parsec.Token as T
import Text.Parsec.Language (haskell)
import Control.Applicative ( (<$> ))
import Control.Monad ( foldM )
import Data.Maybe ( isJust )

{-

identify year name = 
  text ("tc_" ++ show year ++ "_" ++ T.unpack name)
  
code_part year name = 
  identify year name <+> "=" <+> output (extract year name)

code year =
  let names = participant_names $ the_competition year
      preamble =
        text ("tc_"++ show year ++ " = collect " ++ show year)
           <+> P.list (map (identify year) names)
  in  vcat $ preamble : do
        name <- names
        return $ vcat [ "--" , code_part year name ]

bare :: Year -> Competition [Participant]           
bare year = fmap (const []) $ the_competition year

skeleton year =
  fmap (\ci -> ci { participants=[]}) $ the_competition year

-}

collect :: Competition Catinfo
        -> [Competition [Participant]]
        -> Competition Catinfo
collect base cs =
  let computation = do
        let start = fmap (const[]) base
            insertC c d = do [e] <- insert [c] [d]; return e
        c <- foldM insertC start cs 
        fill base c 
  in  case computation of
    Right c -> c
    Left err -> error err



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


