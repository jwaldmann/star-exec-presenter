module Handler.LegacyShowSolverInfo where

import Import
import Network.HTTP.Types.Status

getLegacyShowSolverInfoR :: SolverID -> Handler Html
getLegacyShowSolverInfoR = (redirectWith movedPermanently301) . ShowSolverInfoR
