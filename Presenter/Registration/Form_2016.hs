module Presenter.Registration.Form_2016 where

import Presenter.Registration.Code
import Presenter.Registration.Data

import Data.Maybe

-- FIXME: change to newer TPDB version when it becomes available
tc_skeleton :: Competition Catinfo
tc_skeleton = tc_skeleton_10_4

plain = 317
ceta  = 323

tc_skeleton_10_4 :: Competition Catinfo
tc_skeleton_10_4 = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "TRS Standard"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185245] , participants = []}
          ,Category "SRS Standard"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185159] , participants = []}
          ,Category "Cycles"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185159] , participants = []}
          ,Category "TRS Relative"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185331] , participants = []}
          ,Category "SRS Relative"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185307] , participants = []}
          ,Category "TRS Standard certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 185245] , participants = []}
          ,Category "SRS Standard certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 185159] , participants = []}
          ,Category "TRS Relative certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 185331] , participants = []}
          ,Category "SRS Relative certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 185307] , participants = []}
          ,Category "TRS Equational"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185319] , participants = []}
          ,Category "TRS Equational certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 185319] , participants = []}
          ,Category "TRS Conditional"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185335] , participants = []}
          ,Category "TRS Context Sensitive"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185155] , participants = []}
          ,Category "TRS Innermost"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185292] , participants = []}
          ,Category "TRS Outermost"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185315] , participants = []}
          ,Category "TRS Innermost certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 185292] , participants = []}
          ,Category "TRS Outermost certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 185315] , participants = []}
          ,Category "Higher-Order rewriting (union beta)"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185174] , participants = []}
          ,Category "Integer Transition Systems"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185064] , participants = []}
          ,Category "Integer TRS"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185324] , participants = []}]
     ,MetaCategory "Complexity Analysis of Term Rewriting"
          [Category "Derivational Complexity - Full Rewriting"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185207] , participants = []}
          ,Category "Runtime Complexity - Full Rewriting"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185180] , participants = []}
          ,Category "Runtime Complexity - Innermost Rewriting"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185033] , participants = []}
          ,Category "Derivational Complexity - Full Rewriting certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 185207] , participants = []}
          ,Category "Runtime Complexity - Full Rewriting certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 185180] , participants = []}
          ,Category "Runtime Complexity - Innermost Rewriting certified"
               Catinfo {postproc = ceta , benchmarks = [Hierarchy 185033] , participants = []}
          ,Category "Complexity - Integer Transition Systems"
               Catinfo { postproc = plain, benchmarks = [Hierarchy 185064], participants = []}
          ]
     ,MetaCategory "Termination of Programming Languages"
          [Category "C"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185025] , participants = []}
          ,Category "C Integer Programs"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185312] , participants = []}
          ,Category "Java Bytecode"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185280, Hierarchy 185300] , participants = []}
          ,Category "Logic Programming"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185014, Hierarchy 185326, Hierarchy 185067], participants = []}
          ,Category "Functional Programming"
               Catinfo {postproc = plain , benchmarks = [Hierarchy 185011] , participants = []}
          ]
     , MetaCategory "Complexity Analysis of Programming Languages"
          [ Category "Complexity - C Integer Programs"
               Catinfo { postproc = plain, benchmarks = [Hierarchy 185298 ], participants = [] }
          ]
     ]

tc :: Competition Catinfo
-- solver, with name of person who registered it
tc = collect tc_skeleton
                         [ tc_AProVE -- David Korzeniewski
                         , tc_Loopus -- Moritz Sinn
                         -- ,tc_AutoNon
                         ,tc_Ctrl -- Cynthia Kop
                         -- ,tc_HipTNT
                         , tc_NaTT -- Akihisa Yamada
                         -- ,tc_T2
                         -- ,tc_TCT2
                         ,tc_TCT -- Michael Schaper
                         ,tc_TTT2 -- Christian Sternagel
                         ,tc_UltimateBuchiAutomizer -- Matthias Heizmann
                         ,tc_Wanda -- Cynthia Kop
                         ,tc_cycsrs -- David Sabel
                         ,tc_matchbox -- Johannes Waldmann
                         ,tc_muterm -- Raul Gutierrez
                         , tc_verymax -- Daniel Larraz
                         , tc_cycnta -- Alexander Fedotov
                         , tc_kflmnm -- Dieter Hofbauer
                         , tc_cofloco -- Antonio Flores Montoya
                         ]

tc_Loopus :: Competition [Participant]
tc_Loopus = Competition "Termination Competition 2016"
    [ MetaCategory "Complexity Analysis of Term Rewriting"
        [ Category "Complexity - Integer Transition Systems"
          [Participant { participantName = "Loopus", solver_config = Just (184687, 9317, 165483)  } ]
        ]
    , MetaCategory "Complexity Analysis of Programming Languages"
          [ Category "Complexity - C Integer Programs"
          [Participant { participantName = "Loopus", solver_config = Just (184687, 9317, 165484) } ]
          ]
    ]

tc_AProVE :: Competition [Participant]
tc_AProVE = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "TRS Standard"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,163935)}]
          ,Category "SRS Standard"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,163935)}]
          ,Category "TRS Relative"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,163935)}]
          ,Category "SRS Relative"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,163935)}]
          ,Category "TRS Standard certified"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,164103)}]
          ,Category "SRS Standard certified"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,164103)}]
          ,Category "TRS Relative certified"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,164103)}]
          ,Category "SRS Relative certified"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,164103)}]
          ,Category "TRS Equational"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,163935)}]
          ,Category "TRS Equational certified"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,164103)}]
          ,Category "TRS Conditional"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,163935)}]
          ,Category "TRS Context Sensitive"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,163935)}]
          ,Category "TRS Innermost"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,163935)}]
          ,Category "TRS Outermost"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,163935)}]
          ,Category "TRS Innermost certified"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,164103)}]
          ,Category "TRS Outermost certified"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,164103)}]
          ,Category "Integer Transition Systems"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,163934)}]
          ,Category "Integer TRS"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,163935)}]]
     ,MetaCategory "Complexity Analysis of Term Rewriting"
          [Category "Runtime Complexity - Full Rewriting"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,164592)}]
          ,Category "Runtime Complexity - Innermost Rewriting"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,164592)}]
          ,Category "Runtime Complexity - Innermost Rewriting certified"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,164593)}]
          ,Category "Complexity - Integer Transition Systems"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,163932)}]]
     ,MetaCategory "Termination of Programming Languages"
          [Category "C"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9052,163965)}]
          ,Category "C Integer Programs"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9052,163965)}]
          ,Category "Java Bytecode"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,163935)}]
          ,Category "Logic Programming"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,163935)}]
          ,Category "Functional Programming"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9040,163935)}]]
     ,MetaCategory "Complexity Analysis of Programming Languages"
          [Category "Complexity - C Integer Programs"
               [Participant {participantName = "AProVE" , solver_config = Just (18956,9082,164151)}]]]
  
tc_AutoNon :: Competition [Participant]
tc_AutoNon = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
     [Category "TRS Standard"
          [Participant {participantName = "AutoNon" , solver_config = Just (99108,3865,24242)}]
     ,Category "SRS Standard"
          [Participant {participantName = "AutoNon" , solver_config = Just (99108,3865,24242)}]]]

tc_Ctrl :: Competition [Participant]
tc_Ctrl = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
     [Category "Integer Transition Systems"
          [Participant {participantName = "Ctrl" , solver_config = Just (55489, 3723,23757)}]
     ,Category "Integer TRS"
          [Participant {participantName = "Ctrl" , solver_config = Just (55489, 3723,23758)}]]]

tc_HipTNT :: Competition [Participant]
tc_HipTNT = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Programming Languages"
          [Category "C"
               [Participant {participantName = "HipTNT+" , solver_config = Just (100354, 3902,24352)}]
          ,Category "C Integer Programs"
               [Participant {participantName = "HipTNT+" , solver_config = Just (100354, 3902,24352)}]]]

tc_NaTT :: Competition [Participant]
tc_NaTT = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
     [Category "TRS Standard"
          [Participant {participantName = "NaTT" , solver_config = Just (20344, 9102,164526)}]
     ,Category "SRS Standard"
          [Participant {participantName = "NaTT" , solver_config = Just (20344, 9102,164527)}]
     ,Category "TRS Relative"
          [Participant {participantName = "NaTT" , solver_config = Just (20344, 9102,164529)}]
     ,Category "TRS Equational"
          [Participant {participantName = "NaTT" , solver_config = Just (20344, 9102,164526)}]
     ,Category "TRS Equational certified"
          [Participant {participantName = "NaTT" , solver_config = Just (20344, 9102,164528)}]
     ]]

tc_T2 :: Competition [Participant]
tc_T2 = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
     [Category "Integer Transition Systems"
     [Participant {participantName = "T2" , solver_config = Just (57120, 3509,23138)}]]]

tc_TCT :: Competition [Participant]
tc_TCT = Competition "Termination Competition 2016"
     [MetaCategory "Complexity Analysis of Term Rewriting"
          [Category "Derivational Complexity - Full Rewriting"
               [Participant {participantName = "TcT" , solver_config = Just (24022,9299,165442)}]
          ,Category "Runtime Complexity - Full Rewriting"
               [Participant {participantName = "TcT" , solver_config = Just (24022,9299,165444)}]
          ,Category "Runtime Complexity - Innermost Rewriting"
               [Participant {participantName = "TcT" , solver_config = Just (24022,9299,165446)}]
          ,Category "Derivational Complexity - Full Rewriting certified"
               [Participant {participantName = "TcT" , solver_config = Just (24022,9299,165443)}]
          ,Category "Runtime Complexity - Full Rewriting certified"
               [Participant {participantName = "TcT" , solver_config = Just (24022,9299,165445)}]
          ,Category "Runtime Complexity - Innermost Rewriting certified"
               [Participant {participantName = "TcT" , solver_config = Just (24022,9299,165447)}]
          ,Category "Complexity - Integer Transition Systems"
               [Participant {participantName = "TcT" , solver_config = Just (24022,9299,165441)}]]
     ,MetaCategory "Complexity Analysis of Programming Languages"
          [Category "Complexity - C Integer Programs"
               [Participant {participantName = "TcT" , solver_config = Just (24022,9299,165440)}]]]

tc_TTT2 :: Competition [Participant]
tc_TTT2 = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "TRS Standard"
               [Participant {participantName = "TTT2" , solver_config = Just (184625,9092,164385)}]
          ,Category "SRS Standard"
               [Participant {participantName = "TTT2" , solver_config = Just (184625,9092,164385)}]
          ,Category "TRS Relative"
               [Participant {participantName = "TTT2" , solver_config = Just (184625,9092,164385)}]
          ,Category "SRS Relative"
               [Participant {participantName = "TTT2" , solver_config = Just (184625,9092,164385)}]
          ,Category "TRS Standard certified"
               [Participant {participantName = "TTT2" , solver_config = Just (184625,9092,164384)}]
          ,Category "SRS Standard certified"
               [Participant {participantName = "TTT2" , solver_config = Just (184625,9092,164384)}]
          ,Category "TRS Relative certified"
               [Participant {participantName = "TTT2" , solver_config = Just (184625,9092,164384)}]
          ,Category "SRS Relative certified"
               [Participant {participantName = "TTT2" , solver_config = Just (184625,9092,164384)}]]]


tc_UltimateBuchiAutomizer :: Competition [Participant]
tc_UltimateBuchiAutomizer = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Programming Languages"
     [Category "C"
          [Participant {participantName = "UltimateBuchiAutomizer" , solver_config = Just (51412, 9354,165938)}]
     ,Category "C Integer Programs"
          [Participant {participantName = "UltimateBuchiAutomizer" , solver_config = Just (51412, 9354,165938)}]]]

tc_Wanda :: Competition [Participant]
tc_Wanda = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
     [Category "TRS Standard"
          [Participant {participantName = "Wanda" , solver_config = Just (55489, 1542,2389)}]
     ,Category "Higher-Order rewriting (union beta)"
          [Participant {participantName = "Wanda" , solver_config = Just (55489, 1542,2390)}]]]

tc_cycsrs :: Competition [Participant]
tc_cycsrs = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "Cycles"
               [Participant {participantName = "cycsrs" , solver_config = Just (98142,9005,163520)}]]]

tc_matchbox :: Competition [Participant]
tc_matchbox = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "SRS Standard"
               [Participant {participantName = "matchbox" , solver_config = Just (62205, 9303,165453)}]
          ,Category "Cycles"
               [Participant {participantName = "matchbox" , solver_config = Just (62205, 9303,165454)}]
          ]
     ]

tc_kflmnm :: Competition [Participant]
tc_kflmnm = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "SRS Standard"
               [Participant {participantName = "MultumNonMulta" , solver_config = Just (184509, 9343, 165702)}]
          ,Category "SRS Relative"
               [Participant {participantName = "MultumNonMulta" , solver_config = Just (184509, 9343, 165702)}]
          ]
     ]

tc_muterm :: Competition [Participant]
tc_muterm = Competition "Termination Competition 2016"
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
tc_verymax = Competition "Termination Competition 2016"
     [ MetaCategory "Termination of Term Rewriting (and Transition Systems)"
       [Category "Integer Transition Systems"
        [Participant {participantName = "VeryMax" , solver_config = Just (184540,9339,165578)}]]
     , MetaCategory "Termination of Programming Languages"
       [Category "C Integer Programs"
        [Participant {participantName = "VeryMax" ,solver_config = Just (184540,9339,165578)}]]
     ]

tc_cycnta :: Competition [Participant]
tc_cycnta = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
      [Category "Cycles"
       [Participant {participantName = "CycNTA" , solver_config = Just (184626,9351,165820)}]]]

tc_cofloco :: Competition [Participant]
tc_cofloco = Competition "Termination Competition 2016"
   [ MetaCategory "Complexity Analysis of Term Rewriting"
     [ Category "Complexity - Integer Transition Systems"
       [ Participant {participantName = "CoFloCo" , solver_config = Just (184632,9105,164532)}]]
   , MetaCategory "Complexity Analysis of Programming Languages"
     [ Category "Complexity - C Integer Programs"
       [ Participant {participantName = "CoFloCo" , solver_config = Just (184632,9105,164533)}]
     ]
   ] 
