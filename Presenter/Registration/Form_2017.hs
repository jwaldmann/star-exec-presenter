module Presenter.Registration.Form_2017 where

import Presenter.Registration.Code
import Presenter.Registration.Data

import Data.Maybe
import Prelude (($), (++), take)

-- FIXME: change to newer TPDB version when it becomes available
tc_skeleton :: Competition Catinfo
tc_skeleton = tc_skeleton_10_5

-- Postprocessors:
plain = 355
ceta  = 362

tc_skeleton_10_5 :: Competition Catinfo
tc_skeleton_10_5 = Competition "Termination Competition 2017"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "TRS Standard"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277879] , participants = []}
          ,Category "SRS Standard"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277929] , participants = []}
          ,Category "Cycles"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277834] , participants = []}
          ,Category "TRS Relative"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277914] , participants = []}
          ,Category "SRS Relative"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277987] , participants = []}
          ,Category "TRS Standard certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 277879] , participants = []}
          ,Category "SRS Standard certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 277929] , participants = []}
          ,Category "TRS Relative certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 277914] , participants = []}
          ,Category "SRS Relative certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 277987] , participants = []}
          ,Category "TRS Equational"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277944] , participants = []}
          ,Category "TRS Equational certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 277944] , participants = []}
          ,Category "TRS Conditional"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277830] , participants = []}
          ,Category "TRS Context Sensitive"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277875] , participants = []}
          ,Category "TRS Innermost"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277842] , participants = []}
          ,Category "TRS Outermost"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277806] , participants = []}
          ,Category "TRS Innermost certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 277842] , participants = []}
          ,Category "TRS Outermost certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 277806] , participants = []}
          ,Category "Higher-Order rewriting (union beta)"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277796] , participants = []}
          ,Category "Integer Transition Systems"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277729] , participants = []}
          ,Category "Integer Transition Systems certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 277729] , participants = []}
          ,Category "Integer TRS Innermost"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277804] , participants = []}]
     ,MetaCategory "Complexity Analysis of Term Rewriting"
          [Category "Derivational Complexity - Full Rewriting"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277949] , participants = []}
          ,Category "Runtime Complexity - Full Rewriting"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277848] , participants = []}
          ,Category "Runtime Complexity - Innermost Rewriting"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 278016, Hierarchy 277992] , participants = []}
          ,Category "Derivational Complexity - Full Rewriting certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 277949] , participants = []}
          ,Category "Runtime Complexity - Full Rewriting certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 277848] , participants = []}
          ,Category "Runtime Complexity - Innermost Rewriting certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 278016, Hierarchy 277992] , participants = []}
          ,Category "Complexity - Integer Transition Systems"
               Catinfo { postproc = plain, benchmarks = [Hierarchy 277685], participants = []}
          ]
     ,MetaCategory "Termination of Programming Languages"
          [Category "C"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277822] , participants = []}
          ,Category "C Integer Programs"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277918] , participants = []}
          ,Category "Java Bytecode"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277810, Hierarchy 277719] , participants = []}
          ,Category "Logic Programming"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277732, Hierarchy 277837, Hierarchy 277743], participants = []}
          ,Category "Functional Programming"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 277681] , participants = []}
          ]
     , MetaCategory "Complexity Analysis of Programming Languages"
          [ Category "Complexity - C Integer Programs"
               Catinfo { postproc = plain, benchmarks = [Hierarchy 277918 ], participants = [] }
          ]
     ]

-- | solver, with name of person who registered it
tc :: Competition Catinfo
tc = collect tc_skeleton $
                         [ tc_cofloco -- Antonio Flores Montoya
                         , tc_AProVE -- Florian Frohn
                         -- , tc_Loopus -- Moritz Sinn
                         -- , tc_AutoNon
                         , tc_Ctrl -- Cynthia Kop
                         -- , tc_HipTNT
                         , tc_NaTT -- Akihisa Yamada
                         -- , tc_T2
                         -- , tc_TCT2
                         , tc_TCT -- Georg Moser
                         -- , tc_TTT2 -- Christian Sternagel
                         , tc_UltimateBuchiAutomizer -- Matthias Heizmann
                         , tc_Wanda -- Cynthia Kop
                         -- , tc_cycsrs -- David Sabel
                         , tc_matchbox -- Johannes Waldmann
                         , tc_muterm -- Raul Gutierrez
                         , tc_verymax -- Albert Rubio
                         -- , tc_cycnta -- Alexander Fedotov
                         , tc_kflmnm -- Dieter Hofbauer
                         , tc_sol -- Makoto Hamana
                         ]

tc_Loopus :: Competition [Participant]
tc_Loopus = Competition "Termination Competition 2017"
    [ MetaCategory "Complexity Analysis of Term Rewriting"
        [ Category "Complexity - Integer Transition Systems"
          [Participant { participantName = "Loopus", solver_config = Nothing  } ]
        ]
    , MetaCategory "Complexity Analysis of Programming Languages"
          [ Category "Complexity - C Integer Programs"
          [Participant { participantName = "Loopus", solver_config = Nothing } ]
          ]
    ]

tc_AProVE :: Competition [Participant]
tc_AProVE =  Competition "Termination Competition 2017"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "TRS Standard"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225865)}]
          ,Category "SRS Standard"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225865)}]
          ,Category "TRS Relative"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225865)}]
          ,Category "SRS Relative"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225865)}]
          ,Category "TRS Standard certified"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225866)}]
          ,Category "SRS Standard certified"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225866)}]
          ,Category "TRS Relative certified"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225866)}]
          ,Category "SRS Relative certified"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225866)}]
          ,Category "TRS Equational"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225865)}]
          ,Category "TRS Equational certified"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225866)}]
          ,Category "TRS Conditional"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225865)}]
          ,Category "TRS Context Sensitive"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225865)}]
          ,Category "TRS Innermost"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225865)}]
          ,Category "TRS Outermost"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225865)}]
          ,Category "TRS Innermost certified"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225866)}]
          ,Category "TRS Outermost certified"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225866)}]
          ,Category "Integer Transition Systems"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225864)}]
          ,Category "Integer Transition Systems certified"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225861)}]
          ,Category "Integer TRS Innermost"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225865)}]]
     ,MetaCategory "Complexity Analysis of Term Rewriting"
          [Category "Runtime Complexity - Full Rewriting"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225863)}]
          ,Category "Runtime Complexity - Innermost Rewriting"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225863)}]
          ,Category "Runtime Complexity - Innermost Rewriting certified"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225866)}]
          ,Category "Complexity - Integer Transition Systems"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225863)}]]
     ,MetaCategory "Termination of Programming Languages"
          [Category "C"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225867)}]
          ,Category "C Integer Programs"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225867)}]
          ,Category "Java Bytecode"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225865)}]
          ,Category "Logic Programming"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225865)}]
          ,Category "Functional Programming"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225865)}]]
     ,MetaCategory "Complexity Analysis of Programming Languages"
          [Category "Complexity - C Integer Programs"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,12843,225862)}]]]
     
tc_AutoNon :: Competition [Participant]
tc_AutoNon = Competition "Termination Competition 2017"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
     [Category "TRS Standard"
          [Participant {participantName = "AutoNon" , solver_config = Nothing}]
     ,Category "SRS Standard"
          [Participant {participantName = "AutoNon" , solver_config = Nothing}]]]

tc_Ctrl :: Competition [Participant]
tc_Ctrl = Competition "Termination Competition 2017"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "Integer Transition Systems"
               [Participant {participantName = "Ctrl" , solver_config = Just (55489,3723,23757)}]
          ,Category "Integer TRS Innermost"
               [Participant {participantName = "Ctrl" , solver_config = Just (55489,3723,23758)}]]]

tc_HipTNT :: Competition [Participant]
tc_HipTNT = Competition "Termination Competition 2017"
     [MetaCategory "Termination of Programming Languages"
          [Category "C"
               [Participant {participantName = "HipTNT+" , solver_config = Nothing}]
          ,Category "C Integer Programs"
               [Participant {participantName = "HipTNT+" , solver_config = Nothing}]]]

tc_NaTT :: Competition [Participant]
tc_NaTT = Competition "Termination Competition 2017"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "TRS Standard"
               [Participant {participantName = "NaTT" , solver_config = Just (20344,12862,227231)}]
          ,Category "SRS Standard"
               [Participant {participantName = "NaTT" , solver_config = Just (20344,12862,227231)}]
          ,Category "TRS Relative"
               [Participant {participantName = "NaTT" , solver_config = Just (20344,12862,227231)}]
          ,Category "SRS Relative"
               [Participant {participantName = "NaTT" , solver_config = Just (20344,12862,227231)}]
          ,Category "TRS Equational"
               [Participant {participantName = "NaTT" , solver_config = Just (20344,12862,227231)}]
          ,Category "TRS Equational certified"
               [Participant {participantName = "NaTT" , solver_config = Just (20344,12862,227233)}]]]

tc_T2 :: Competition [Participant]
tc_T2 = Competition "Termination Competition 2017"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
     [Category "Integer Transition Systems"
     [Participant {participantName = "T2" , solver_config = Nothing}]]]

tc_TCT :: Competition [Participant]
tc_TCT = Competition "Termination Competition 2017"
     [MetaCategory "Complexity Analysis of Term Rewriting"
          [Category "Runtime Complexity - Innermost Rewriting"
               [Participant {participantName = "TcT" , solver_config = Just (24022,12837,225852)}]
          ,Category "Runtime Complexity - Innermost Rewriting certified"
               [Participant {participantName = "TcT" , solver_config = Just (24022,12837,225853)}]]]

tc_TTT2 :: Competition [Participant]
tc_TTT2 = Competition "Termination Competition 2017"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "TRS Standard"
               [Participant {participantName = "TTT2" , solver_config = Nothing}]
          ,Category "SRS Standard"
               [Participant {participantName = "TTT2" , solver_config = Nothing}]
          ,Category "TRS Relative"
               [Participant {participantName = "TTT2" , solver_config = Nothing}]
          ,Category "SRS Relative"
               [Participant {participantName = "TTT2" , solver_config = Nothing}]
          ,Category "TRS Standard certified"
               [Participant {participantName = "TTT2" , solver_config = Nothing}]
          ,Category "SRS Standard certified"
               [Participant {participantName = "TTT2" , solver_config = Nothing}]
          ,Category "TRS Relative certified"
               [Participant {participantName = "TTT2" , solver_config = Nothing}]
          ,Category "SRS Relative certified"
               [Participant {participantName = "TTT2" , solver_config = Nothing}]]]


tc_UltimateBuchiAutomizer :: Competition [Participant]
tc_UltimateBuchiAutomizer = Competition "Termination Competition 2017"
     [MetaCategory "Termination of Programming Languages"
     [Category "C"
          [Participant {participantName = "UltimateBuchiAutomizer" , solver_config = Just (51412, 9354,165938)}]
     ,Category "C Integer Programs"
          [Participant {participantName = "UltimateBuchiAutomizer" , solver_config = Just (51412, 9354,165938)}]]]

tc_Wanda :: Competition [Participant]
tc_Wanda = Competition "Termination Competition 2017"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "TRS Standard"
               [Participant {participantName = "Wanda" , solver_config = Just (55489,1542,2389)}]
          ,Category "Higher-Order rewriting (union beta)"
               [Participant {participantName = "Wanda" , solver_config = Just (55489,1542,2390)}]]]

tc_cycsrs :: Competition [Participant]
tc_cycsrs = Competition "Termination Competition 2017"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "Cycles"
               [Participant {participantName = "cycsrs" , solver_config = Nothing}]]]

tc_matchbox :: Competition [Participant]
tc_matchbox = Competition "Termination Competition 2017"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "SRS Standard"
               [Participant {participantName = "matchbox" , solver_config = Just (62205,12788,225711)}]
          ,Category "Cycles"
               [Participant {participantName = "matchbox" , solver_config = Just (62205,12788,225710)}]]]

tc_kflmnm :: Competition [Participant]
tc_kflmnm = Competition "Termination Competition 2017"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "SRS Standard"
               [Participant {participantName = "MultumNonMulta" , solver_config = Just (184509,12864,227325)}]
          ,Category "SRS Relative"
               [Participant {participantName = "MultumNonMulta" , solver_config = Just (184509,12864,227325)}]
          ,Category "SRS Standard certified"
               [Participant {participantName = "MultumNonMulta" , solver_config = Nothing}]
          ,Category "SRS Relative certified"
               [Participant {participantName = "MultumNonMulta" , solver_config = Nothing}]]]

tc_muterm :: Competition [Participant]
tc_muterm = Competition "Termination Competition 2017"
    [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
         [Category "TRS Standard"
              [Participant {participantName = "muterm" , solver_config = Just (34565,9061,163986)}]
         ,Category "SRS Standard"
              [Participant {participantName = "muterm" , solver_config = Just (34565,9061,163986)}]
         ,Category "TRS Equational"
              [Participant {participantName = "muterm" , solver_config = Just (34565,9061,163986)}]
         ,Category "TRS Conditional"
              [Participant {participantName = "muterm" , solver_config = Just (34565,9061,163986)}]
         ,Category "TRS Context Sensitive"
              [Participant {participantName = "muterm" , solver_config = Just (34565,9061,163986)}]
         ,Category "TRS Innermost"
              [Participant {participantName = "muterm" , solver_config = Just (34565,9061,163986)}]]]

tc_verymax :: Competition [Participant]
tc_verymax = Competition "Termination Competition 2017"
     [ MetaCategory "Termination of Term Rewriting (and Transition Systems)"
       [Category "Integer Transition Systems"
        [Participant {participantName = "VeryMax" , solver_config = Just (184540,9339,165578)}]]
     , MetaCategory "Termination of Programming Languages"
       [Category "C Integer Programs"
        [Participant {participantName = "VeryMax" ,solver_config = Just (184540,9339,165578)}]]
     ]

tc_cycnta :: Competition [Participant]
tc_cycnta = Competition "Termination Competition 2017"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
      [Category "Cycles"
       [Participant {participantName = "CycNTA" , solver_config = Nothing}]]]

tc_cofloco :: Competition [Participant]
tc_cofloco = Competition "Termination Competition 2017"
     [MetaCategory "Complexity Analysis of Term Rewriting"
          [Category "Complexity - Integer Transition Systems"
               [Participant {participantName = "CoFloCo" , solver_config = Just (184632,9105,164532)}]]
     ,MetaCategory "Complexity Analysis of Programming Languages"
          [Category "Complexity - C Integer Programs"
               [Participant {participantName = "CoFloCo" , solver_config = Just (184632,9105,164533)}]]] 

tc_sol :: Competition [Participant]
tc_sol = Competition "Termination Competition 2017"
 [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
   [Category "Higher-Order rewriting (union beta)"
      [Participant {participantName = "SOL" ,
                    solver_config = Just (277287,12832,225845)}]]]
