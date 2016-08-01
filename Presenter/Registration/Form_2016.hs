module Presenter.Registration.Form_2016 where

import Presenter.Registration.Code
import Presenter.Registration.Data

import Data.Maybe

-- FIXME: change to newer TPDB version when it becomes available
tc_skeleton :: Competition Catinfo
tc_skeleton = tc_skeleton_10_3

tc_skeleton_10_3 :: Competition Catinfo
tc_skeleton_10_3 = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "TRS Standard"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 102127] , participants = []}
          ,Category "SRS Standard"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 102166] , participants = []}
          ,Category "Cycles"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 102166] , participants = []}
          ,Category "TRS Relative"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 102162] , participants = []}
          ,Category "SRS Relative"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 102223] , participants = []}
          ,Category "TRS Standard certified"
               Catinfo {postproc = 235 , benchmarks = [Hierarchy 102127] , participants = []}
          ,Category "SRS Standard certified"
               Catinfo {postproc = 235 , benchmarks = [Hierarchy 102166] , participants = []}
          ,Category "TRS Relative certified"
               Catinfo {postproc = 235 , benchmarks = [Hierarchy 102162] , participants = []}
          ,Category "SRS Relative certified"
               Catinfo {postproc = 235 , benchmarks = [Hierarchy 102223] , participants = []}
          ,Category "TRS Equational"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 102180] , participants = []}
          ,Category "TRS Conditional"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 102081] , participants = []}
          ,Category "TRS Context Sensitive"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 102123] , participants = []}
          ,Category "TRS Innermost"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 102090] , participants = []}
          ,Category "TRS Outermost"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 102057] , participants = []}
          ,Category "TRS Innermost certified"
               Catinfo {postproc = 235 , benchmarks = [Hierarchy 102090] , participants = []}
          ,Category "TRS Outermost certified"
               Catinfo {postproc = 235 , benchmarks = [Hierarchy 102057] , participants = []}
          ,Category "Higher-Order rewriting (union beta)"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 102049] , participants = []}
          ,Category "Integer Transition Systems"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 101982] , participants = []}
          ,Category "Integer TRS"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 102055] , participants = []}]
     ,MetaCategory "Complexity Analysis of Term Rewriting"
          [Category "Derivational Complexity - Full Rewriting"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 102185] , participants = []}
          ,Category "Runtime Complexity - Full Rewriting"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 102096] , participants = []}
          ,Category "Runtime Complexity - Innermost Rewriting"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 102228] , participants = []}
          ,Category "Derivational Complexity - Full Rewriting certified"
               Catinfo {postproc = 235 , benchmarks = [Hierarchy 102185] , participants = []}
          ,Category "Runtime Complexity - Full Rewriting certified"
               Catinfo {postproc = 235 , benchmarks = [Hierarchy 102096] , participants = []}
          ,Category "Runtime Complexity - Innermost Rewriting certified"
               Catinfo {postproc = 235 , benchmarks = [Hierarchy 102228] , participants = []}
          ,Category "Integer Transition Systems"
               Catinfo { postproc = 234, benchmarks = [Hierarchy 101982], participants = []}
          ]
     ,MetaCategory "Termination of Programming Languages"
          [Category "C"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 102073] , participants = []}
          ,Category "C Integer Programs"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 101979] , participants = []}
          ,Category "Java Bytecode"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 102061
                                                      ,Hierarchy 101972] , participants = []}
          ,Category "Logic Programming"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 101985
                                                      ,Hierarchy 102085
                                                      ,Hierarchy 101996] , participants = []}
          ,Category "Functional Programming"
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 101968] , participants = []}
          ]
     , MetaCategory "Complexity Analysis of Programming Languages"
          [ Category "C Integer Programs"
               Catinfo { postproc = 235, benchmarks = [Hierarchy 101979], participants = [] }
          ]
     ]

tc :: Competition Catinfo
tc = collect tc_skeleton
                         [ tc_AProVE
                         , tc_Loopus
                         -- ,tc_AutoNon
                         -- ,tc_Ctrl
                         -- ,tc_HipTNT
                         -- ,tc_NaTT
                         -- ,tc_T2
                         -- ,tc_TCT2
                         ,tc_TCT3
                         -- ,tc_TTT2
                         -- ,tc_UltimateBuchiAutomizer
                         -- ,tc_UltimateBuchiAutomizerJoogie
                         -- ,tc_Wanda
                         ,tc_cycsrs
                         ,tc_matchbox
                         ,tc_muterm
                         ]

tc_Loopus :: Competition [Participant]
tc_Loopus = Competition "Termination Competition 2016"
    [ MetaCategory "Complexity Analysis of Term Rewriting"
        [ Category "Integer Transition Systems"
          [Participant { participantName = "Loopus", solver_config = Nothing } ]
        ]
    , MetaCategory "Complexity Analysis of Programming Languages"
          [ Category "C Integer Programs"
          [Participant { participantName = "Loopus", solver_config = Nothing } ]
          ]
    ]

tc_AProVE :: Competition [Participant]
tc_AProVE = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "TRS Standard"
               [Participant {participantName = "AProVE" , solver_config = Just (19356, 3870,24272)}]
          ,Category "SRS Standard"
               [Participant {participantName = "AProVE" , solver_config = Just (19356, 3870,24272)}]
          ,Category "TRS Relative"
               [Participant {participantName = "AProVE" , solver_config = Just (19356, 3870,24272)}]
          ,Category "SRS Relative"
               [Participant {participantName = "AProVE" , solver_config = Just (19356, 3870,24272)}]

          ,Category "TRS Standard certified"
           [Participant {participantName = "AProVE" , solver_config = Just (19356, 3931,24733)}]
          ,Category "SRS Standard certified"
           [Participant {participantName = "AProVE" , solver_config = Just (19356, 3931,24733)}]
          ,Category "TRS Relative certified"
           [Participant {participantName = "AProVE" , solver_config = Just (19356, 3931,24733)}]
          ,Category "SRS Relative certified"
           [Participant {participantName = "AProVE" , solver_config = Just (19356, 3931,24733)}]

          ,Category "TRS Equational"
               [Participant {participantName = "AProVE" , solver_config = Just (19356, 3870,24272)}]
          ,Category "TRS Conditional"
               [Participant {participantName = "AProVE" , solver_config = Just (19356, 3870,24272)}]
          ,Category "TRS Context Sensitive"
               [Participant {participantName = "AProVE" , solver_config = Just (19356, 3870,24272)}]
          ,Category "TRS Innermost"
               [Participant {participantName = "AProVE" , solver_config = Just (19356, 3870,24272)}]
          ,Category "TRS Outermost"
               [Participant {participantName = "AProVE" , solver_config = Just (19356, 3870,24272)}]

          ,Category "TRS Innermost certified"
           [Participant {participantName = "AProVE" , solver_config = Just (19356, 3931,24733)}]
          ,Category "TRS Outermost certified"
           [Participant {participantName = "AProVE" , solver_config = Just (19356, 3931,24733)}]

          ,Category "Integer Transition Systems"
               [Participant {participantName = "AProVE" , solver_config = Just (19356, 3870,24266)}]
          ,Category "Integer TRS"
               [Participant {participantName = "AProVE" , solver_config = Just (19356, 3870,24270)}]]
     ,MetaCategory "Complexity Analysis of Term Rewriting"
          [Category "Runtime Complexity - Full Rewriting"
               [Participant {participantName = "AProVE" , solver_config = Just (19356, 3870,24264)}]
          ,Category "Runtime Complexity - Innermost Rewriting"
               [Participant {participantName = "AProVE" , solver_config = Just (19356, 3870,24267)}]
          ,Category "Runtime Complexity - Innermost Rewriting certified"
           [Participant {participantName = "AProVE" , solver_config = Just (19356, 3931,24733)}]]
     ,MetaCategory "Termination of Programming Languages"
          [Category "C"
               [Participant {participantName = "AProVE" , solver_config = Just (19356, 3870,24271)}]
          ,Category "C Integer Programs"
               [Participant {participantName = "AProVE" , solver_config = Just (19356, 3870,24271)}]
          ,Category "Java Bytecode"
               [Participant {participantName = "AProVE" , solver_config = Just (19356, 3870,24273)}]
          ,Category "Logic Programming"
               [Participant {participantName = "AProVE" , solver_config = Just (19356, 3870,24269)}]
          ,Category "Functional Programming"
               [Participant {participantName = "AProVE" , solver_config = Just (19356, 3870,24265)}]]]

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
          [Participant {participantName = "NaTT" , solver_config = Just (20344, 3430,22689)}]
     ,Category "SRS Standard"
          [Participant {participantName = "NaTT" , solver_config = Just (20344, 3430,22691)}]
     ,Category "TRS Relative"
          [Participant {participantName = "NaTT" , solver_config = Just (20344, 3430,22690)}]
     ,Category "SRS Relative"
          [Participant {participantName = "NaTT" , solver_config = Just (20344, 3430,22690)}]]]

tc_T2 :: Competition [Participant]
tc_T2 = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
     [Category "Integer Transition Systems"
     [Participant {participantName = "T2" , solver_config = Just (57120, 3509,23138)}]]]

tc_TCT2 :: Competition [Participant]
tc_TCT2 = Competition "Termination Competition 2016"
     [MetaCategory "Complexity Analysis of Term Rewriting"
     [Category "Derivational Complexity - Full Rewriting"
          [Participant {participantName = "TCT2" , solver_config = Just (52530, 3797,24076)}]
     ,Category "Runtime Complexity - Full Rewriting"
          [Participant {participantName = "TCT2" , solver_config = Just (52530, 3797,24071)}]
     ,Category "Runtime Complexity - Innermost Rewriting"
          [Participant {participantName = "TCT2" , solver_config = Just (52530, 3797,24070)}]
     ,Category "Derivational Complexity - Full Rewriting certified"
          [Participant {participantName = "TCT2" , solver_config = Just (52530, 3797,24074)}]
     ,Category "Runtime Complexity - Full Rewriting certified"
          [Participant {participantName = "TCT2" , solver_config = Just (52530, 3797,24069)}]
     ,Category "Runtime Complexity - Innermost Rewriting certified"
          [Participant {participantName = "TCT2" , solver_config = Just (52530, 3797,24072)}]]]

tc_TCT3 :: Competition [Participant]
tc_TCT3 = Competition "Termination Competition 2016"
     [MetaCategory "Complexity Analysis of Term Rewriting"
     [Category "Derivational Complexity - Full Rewriting"
          [Participant {participantName = "TCT3" , solver_config = Just (52530, 3866,24249)}]
     ,Category "Runtime Complexity - Full Rewriting"
          [Participant {participantName = "TCT3" , solver_config = Just (52530, 3866,24243)}]
     ,Category "Runtime Complexity - Innermost Rewriting"
          [Participant {participantName = "TCT3" , solver_config = Just (52530, 3866,24244)}]
     ,Category "Derivational Complexity - Full Rewriting certified"
          [Participant {participantName = "TCT3" , solver_config = Just (52530, 3866,24245)}]
     ,Category "Runtime Complexity - Full Rewriting certified"
          [Participant {participantName = "TCT3" , solver_config = Just (52530, 3866,24247)}]
     ,Category "Runtime Complexity - Innermost Rewriting certified"
          [Participant {participantName = "TCT3" , solver_config = Just (52530, 3866,24250)}]]]

tc_TTT2 :: Competition [Participant]
tc_TTT2 = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
     [Category "TRS Standard"
          [Participant {participantName = "TTT2" , solver_config = Just (100695, 3558,23357)}]
     ,Category "SRS Standard"
          [Participant {participantName = "TTT2" , solver_config = Just (100695, 3558,23357)}]
     ,Category "TRS Relative"
          [Participant {participantName = "TTT2" , solver_config = Just (100695, 3558,23357)}]
     ,Category "SRS Relative"
          [Participant {participantName = "TTT2" , solver_config = Just (100695, 3558,23357)}]
     ,Category "TRS Standard certified"
          [Participant {participantName = "TTT2" , solver_config = Just (100695, 3558,23358)}]
     ,Category "SRS Standard certified"
          [Participant {participantName = "TTT2" , solver_config = Just (100695, 3558,23358)}]
     ,Category "TRS Relative certified"
          [Participant {participantName = "TTT2" , solver_config = Just (100695, 3558,23358)}]
     ,Category "SRS Relative certified"
          [Participant {participantName = "TTT2" , solver_config = Just (100695, 3558,23358)}]]]

tc_UltimateBuchiAutomizer :: Competition [Participant]
tc_UltimateBuchiAutomizer = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Programming Languages"
     [Category "C"
          [Participant {participantName = "UltimateBuchiAutomizer" , solver_config = Just (51412, 3942,24757)}]
     ,Category "C Integer Programs"
          [Participant {participantName = "UltimateBuchiAutomizer" , solver_config = Just (51412, 3942,24757)}]]]

tc_UltimateBuchiAutomizerJoogie :: Competition [Participant]
tc_UltimateBuchiAutomizerJoogie = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Programming Languages"
     [Category "Java Bytecode"
     [Participant {participantName = "UltimateBuchiAutomizer+Joogie" , solver_config = Just (51412, 3942,24758)}]]]

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
     [Participant {participantName = "cycsrs" , solver_config = Just (98142,3869,24259)}]]]

tc_matchbox :: Competition [Participant]
tc_matchbox = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "SRS Standard"
               [Participant {participantName = "matchbox" , solver_config = Just (62397, 8817,161727)}]
          ,Category "Cycles"
               [Participant {participantName = "matchbox" , solver_config = Just (62397, 8817,161728)}]
          ]
     ]

tc_muterm :: Competition [Participant]
tc_muterm = Competition "Termination Competition 2016"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "TRS Standard"
               [Participant {participantName = "muterm" , solver_config = Just (34565,3897,24307)}]
          ,Category "SRS Standard"
               [Participant {participantName = "muterm" , solver_config = Just (34565,3897,24307)}]
          ,Category "TRS Equational"
               [Participant {participantName = "muterm" , solver_config = Just (34565,3897,24307)}]
          ,Category "TRS Conditional"
               [Participant {participantName = "muterm" , solver_config = Just (34565,3897,24307)}]
          ,Category "TRS Context Sensitive"
               [Participant {participantName = "muterm" , solver_config = Just (34565,3897,24307)}]
          ,Category "TRS Innermost"
               [Participant {participantName = "muterm" , solver_config = Just (34565,3897,24307)}]]]
