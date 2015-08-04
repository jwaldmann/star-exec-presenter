module Presenter.Registration.Form where

import Presenter.Registration.Code
import Presenter.Registration.Data

import Data.Maybe

tc_Y2015_skeleton = tc_Y2015_skeleton_10_3

tc_Y2015_skeleton_10_3 = Competition "Termination Competition 2015"
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
               Catinfo {postproc = 235 , benchmarks = [Hierarchy 102228] , participants = []}]
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
               Catinfo {postproc = 234 , benchmarks = [Hierarchy 101968] , participants = []}]]


tc_Y2015 = collect tc_Y2015_skeleton
                         [tc_Y2015_AProVE
                         ,tc_Y2015_AutoNon
                         ,tc_Y2015_Ctrl
                         ,tc_Y2015_HipTNT
                         ,tc_Y2015_NaTT
                         ,tc_Y2015_T2
                         ,tc_Y2015_TCT2
                         ,tc_Y2015_TCT3
                         ,tc_Y2015_TTT2
                         ,tc_Y2015_UltimateBuchiAutomizer
                         ,tc_Y2015_UltimateBuchiAutomizerJoogie
                         ,tc_Y2015_Wanda
                         ,tc_Y2015_cycsrs
                         ,tc_Y2015_matchbox
                         ,tc_Y2015_muterm]
--
tc_Y2015_AProVE = Competition "Termination Competition 2015"
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
--
tc_Y2015_AutoNon = Competition "Termination Competition 2015"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
     [Category "TRS Standard"
          [Participant {participantName = "AutoNon" , solver_config = Just (99108,3865,24242)}]
     ,Category "SRS Standard"
          [Participant {participantName = "AutoNon" , solver_config = Just (99108,3865,24242)}]]]
--
tc_Y2015_Ctrl = Competition "Termination Competition 2015"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
     [Category "Integer Transition Systems"
          [Participant {participantName = "Ctrl" , solver_config = Just (55489, 3723,23757)}]
     ,Category "Integer TRS"
          [Participant {participantName = "Ctrl" , solver_config = Just (55489, 3723,23758)}]]]
--
tc_Y2015_HipTNT = Competition "Termination Competition 2015"
     [MetaCategory "Termination of Programming Languages"
          [Category "C"
               [Participant {participantName = "HipTNT+" , solver_config = Just (99352, 3902,24352)}]
          ,Category "C Integer Programs"
               [Participant {participantName = "HipTNT+" , solver_config = Just (99352, 3902,24352)}]]]
--
tc_Y2015_NaTT = Competition "Termination Competition 2015"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
     [Category "TRS Standard"
          [Participant {participantName = "NaTT" , solver_config = Just (20344, 3430,22689)}]
     ,Category "SRS Standard"
          [Participant {participantName = "NaTT" , solver_config = Just (20344, 3430,22691)}]
     ,Category "TRS Relative"
          [Participant {participantName = "NaTT" , solver_config = Just (20344, 3430,22690)}]
     ,Category "SRS Relative"
          [Participant {participantName = "NaTT" , solver_config = Just (20344, 3430,22690)}]]]
--
tc_Y2015_T2 = Competition "Termination Competition 2015"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
     [Category "Integer Transition Systems"
     [Participant {participantName = "T2" , solver_config = Just (57120, 3509,23138)}]]]
--
tc_Y2015_TCT2 = Competition "Termination Competition 2015"
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
--
tc_Y2015_TCT3 = Competition "Termination Competition 2015"
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
--
tc_Y2015_TTT2 = Competition "Termination Competition 2015"
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
--
tc_Y2015_UltimateBuchiAutomizer = Competition "Termination Competition 2015"
     [MetaCategory "Termination of Programming Languages"
     [Category "C"
          [Participant {participantName = "UltimateBuchiAutomizer" , solver_config = Just (51412, 3942,24757)}]
     ,Category "C Integer Programs"
          [Participant {participantName = "UltimateBuchiAutomizer" , solver_config = Just (51412, 3942,24757)}]]]
--
tc_Y2015_UltimateBuchiAutomizerJoogie = Competition "Termination Competition 2015"
     [MetaCategory "Termination of Programming Languages"
     [Category "Java Bytecode"
     [Participant {participantName = "UltimateBuchiAutomizer+Joogie" , solver_config = Just (51412, 3942,24758)}]]]
--
tc_Y2015_Wanda = Competition "Termination Competition 2015"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
     [Category "TRS Standard"
          [Participant {participantName = "Wanda" , solver_config = Just (55489, 1542,2389)}]
     ,Category "Higher-Order rewriting (union beta)"
          [Participant {participantName = "Wanda" , solver_config = Just (55489, 1542,2390)}]]]
--
tc_Y2015_cycsrs = Competition "Termination Competition 2015"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
     [Category "Cycles"
     [Participant {participantName = "cycsrs" , solver_config = Just (98142,3869,24259)}]]]
--
tc_Y2015_matchbox = Competition "Termination Competition 2015"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
          [Category "TRS Standard"
               [Participant {participantName = "matchbox" , solver_config = Just (62205, 3804,24112)}]
          ,Category "SRS Standard"
               [Participant {participantName = "matchbox" , solver_config = Just (62205, 3804,24112)}]
          ,Category "Cycles"
               [Participant {participantName = "matchbox" , solver_config = Just (62205, 3804,24110)}]
          ,Category "TRS Relative"
               [Participant {participantName = "matchbox" , solver_config = Just (62205, 3804,24105)}]
          ,Category "SRS Relative"
               [Participant {participantName = "matchbox" , solver_config = Just (62205, 3804,24105)}]]
     ,MetaCategory "Complexity Analysis of Term Rewriting"
          [Category "Derivational Complexity - Full Rewriting"
          [Participant {participantName = "matchbox" , solver_config = Just (62205, 3804,24117)}]]]
--
tc_Y2015_muterm = Competition "Termination Competition 2015"
     [MetaCategory "Termination of Term Rewriting (and Transition Systems)"
     [Category "TRS Standard"
          [Participant {participantName = "muterm" , solver_config = Just (34565, 3897,24307)}]
     ,Category "SRS Standard"
          [Participant {participantName = "muterm" , solver_config = Just (34565, 3897,24307)}]
     ,Category "TRS Equational"
          [Participant {participantName = "muterm" , solver_config = Just (34565, 3897,24307)}]
     ,Category "TRS Conditional"
          [Participant {participantName = "muterm" , solver_config = Just (34565, 3897,24307)}]
     ,Category "TRS Context Sensitive"
          [Participant {participantName = "muterm" , solver_config = Just (34565, 3897,24307)}]
     ,Category "TRS Innermost"
          [Participant {participantName = "muterm" , solver_config = Just (34565, 3897,24307)}]]]
