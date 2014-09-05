module Handler.LegacyShowSolverInfo where

import Import
import Handler.ShowSolverInfo

getLegacyShowSolverInfoR :: SolverID -> Handler Html
getLegacyShowSolverInfoR = getShowSolverInfoR
