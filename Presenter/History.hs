module Presenter.History where

import Presenter.Model.RouteTypes (JobID (..))
import Presenter.Model.Competition

termcomp2014 = Competition
     { getMetaData =    CompetitionMeta
                             { getMetaName =    "Termination Competition 2014"
                             , getMetaDescription =    "wc = 300 a = 100 b = 1000 c = 1.0 ( 2014-07-20 02:25:05.328531 UTC )"
                             }
     , getMetaCategories =    [MetaCategory
                                   { getMetaCategoryName =    "Termination of Term Rewriting (and Transition Systems)"
                                   , getCategories =    [Category
                                                             { getCategoryName =    "TRS Standard"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5373]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "SRS Standard"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5374]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "TRS Relative"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5375]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "SRS Relative"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5376]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "TRS Standard certified"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    172
                                                             , getJobIds =    [StarExecJobID 5377]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "SRS Standard certified"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    172
                                                             , getJobIds =    [StarExecJobID 5378]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "TRS Relative certified"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    172
                                                             , getJobIds =    [StarExecJobID 5379]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "SRS Relative certified"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    172
                                                             , getJobIds =    [StarExecJobID 5380]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "TRS Equational"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5381]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "TRS Conditional"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5382]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "TRS Context Sensitive"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5383]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "TRS Innermost"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5384]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "Higher-Order rewriting (union beta)"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5385]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "Integer Transition Systems"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5386]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "Integer TRS"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5387]
                                                             }]
                                   }
                              ,MetaCategory
                                   { getMetaCategoryName =    "Complexity Analysis of Term Rewriting"
                                   , getCategories =    [Category
                                                             { getCategoryName =    "Derivational Complexity - Full Rewriting"
                                                             , getCategoryScoring =    Complexity
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5388]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "Runtime Complexity - Full Rewriting"
                                                             , getCategoryScoring =    Complexity
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5389]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "Runtime Complexity - Innermost Rewriting"
                                                             , getCategoryScoring =    Complexity
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5390]
                                                             }]
                                   }
                              ,MetaCategory
                                   { getMetaCategoryName =    "Termination of Programming Languages"
                                   , getCategories =    [Category
                                                             { getCategoryName =    "C"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5391]
                                                             }]
                                   }]
     }

termcomp2014_demonstration = Competition
     { getMetaData =    CompetitionMeta
                             { getMetaName =    "Termination Competition 2014 (Demonstration)"
                             , getMetaDescription =    "wc = 300 a = 100 b = 1000 c = 1.0 ( 2014-07-21 08:00:05.917226 UTC )"
                             }
     , getMetaCategories =    [MetaCategory
                                   { getMetaCategoryName =    "Termination of Term Rewriting (and Transition Systems)"
                                   , getCategories =    [Category
                                                             { getCategoryName =    "TRS Outermost"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5412]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "TRS Innermost certified"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    172
                                                             , getJobIds =    [StarExecJobID 5413]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "TRS Outermost certified"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    172
                                                             , getJobIds =    [StarExecJobID 5414]
                                                             }]
                                   }
                              ,MetaCategory
                                   { getMetaCategoryName =    "Complexity Analysis of Term Rewriting"
                                   , getCategories =    [Category
                                                             { getCategoryName =    "Derivational Complexity - Full Rewriting certified"
                                                             , getCategoryScoring =    Complexity
                                                             , getPostProcId =    172
                                                             , getJobIds =    [StarExecJobID 5415]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "Runtime Complexity - Full Rewriting certified"
                                                             , getCategoryScoring =    Complexity
                                                             , getPostProcId =    172
                                                             , getJobIds =    [StarExecJobID 5416]
                                                             }]
                                   }
                              ,MetaCategory
                                   { getMetaCategoryName =    "Termination of Programming Languages"
                                   , getCategories =    [Category
                                                             { getCategoryName =    "Java"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5417]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "Logic Programming"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5418]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "Functional Programming"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    163
                                                             , getJobIds =    [StarExecJobID 5419]
                                                             }]
                                   }]
     }

termcomp2015 = Competition
     { getMetaData =    CompetitionMeta
                             { getMetaName =    "Termination Competition 2015"
                             , getMetaDescription =    "wc = 300 a = 1 b = 1 c = 0.1 ( 2015-08-05 18:48:18.60928 UTC )"
                             }
     , getMetaCategories =    [MetaCategory
                                   { getMetaCategoryName =    "Termination of Term Rewriting (and Transition Systems)"
                                   , getCategories =    [Category
                                                             { getCategoryName =    "TRS Standard"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10257]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "SRS Standard"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10296]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "Cycles"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10259]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "TRS Relative"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10298]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "SRS Relative"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10299]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "TRS Standard certified"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    235
                                                             , getJobIds =    [StarExecJobID 10256]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "SRS Standard certified"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    235
                                                             , getJobIds =    [StarExecJobID 10301]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "TRS Relative certified"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    235
                                                             , getJobIds =    [StarExecJobID 10302]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "SRS Relative certified"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    235
                                                             , getJobIds =    [StarExecJobID 10303]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "TRS Equational"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10304]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "TRS Conditional"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10305]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "TRS Context Sensitive"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10306]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "TRS Innermost"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10307]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "Integer Transition Systems"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10308]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "Integer TRS"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10309]
                                                             }]
                                   }
                              ,MetaCategory
                                   { getMetaCategoryName =    "Complexity Analysis of Term Rewriting"
                                   , getCategories =    [Category
                                                             { getCategoryName =    "Derivational Complexity - Full Rewriting"
                                                             , getCategoryScoring =    Complexity
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10310]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "Runtime Complexity - Full Rewriting"
                                                             , getCategoryScoring =    Complexity
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10311]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "Runtime Complexity - Innermost Rewriting"
                                                             , getCategoryScoring =    Complexity
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10312]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "Runtime Complexity - Innermost Rewriting certified"
                                                             , getCategoryScoring =    Complexity
                                                             , getPostProcId =    235
                                                             , getJobIds =    [StarExecJobID 10313]
                                                             }]
                                   }
                              ,MetaCategory
                                   { getMetaCategoryName =    "Termination of Programming Languages"
                                   , getCategories =    [Category
                                                             { getCategoryName =    "C"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10260]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "C Integer Programs"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10261]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "Java Bytecode"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10316
                                                                              ,StarExecJobID 10317]
                                                             }]
                                   }]
     }

termcomp2015_demonstration = Competition
     { getMetaData =    CompetitionMeta
                             { getMetaName =    "Termination Competition 2015 (Demonstration)"
                             , getMetaDescription =    "wc = 300 a = 1 b = 1 c = 0.1 ( 2015-08-05 19:04:13.66475 UTC )"
                             }
     , getMetaCategories =    [MetaCategory
                                   { getMetaCategoryName =    "Termination of Term Rewriting (and Transition Systems)"
                                   , getCategories =    [Category
                                                             { getCategoryName =    "TRS Outermost"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10318]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "TRS Innermost certified"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    235
                                                             , getJobIds =    [StarExecJobID 10319]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "TRS Outermost certified"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    235
                                                             , getJobIds =    [StarExecJobID 10320]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "Higher-Order rewriting (union beta)"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10321]
                                                             }]
                                   }
                              ,MetaCategory
                                   { getMetaCategoryName =    "Complexity Analysis of Term Rewriting"
                                   , getCategories =    [Category
                                                             { getCategoryName =    "Derivational Complexity - Full Rewriting certified"
                                                             , getCategoryScoring =    Complexity
                                                             , getPostProcId =    235
                                                             , getJobIds =    [StarExecJobID 10322]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "Runtime Complexity - Full Rewriting certified"
                                                             , getCategoryScoring =    Complexity
                                                             , getPostProcId =    235
                                                             , getJobIds =    [StarExecJobID 10323]
                                                             }]
                                   }
                              ,MetaCategory
                                   { getMetaCategoryName =    "Termination of Programming Languages"
                                   , getCategories =    [Category
                                                             { getCategoryName =    "Logic Programming"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10324
                                                                              ,StarExecJobID 10325
                                                                              ,StarExecJobID 10326]
                                                             }
                                                        ,Category
                                                             { getCategoryName =    "Functional Programming"
                                                             , getCategoryScoring =    Standard
                                                             , getPostProcId =    234
                                                             , getJobIds =    [StarExecJobID 10327]
                                                             }]
                                   }]
     }