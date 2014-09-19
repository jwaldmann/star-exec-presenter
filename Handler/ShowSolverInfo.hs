module Handler.ShowSolverInfo where

import Import
import Presenter.StarExec.JobData
import Presenter.Internal.Stringish
import Presenter.Utils.WidgetMetaRefresh
import Presenter.PersistHelper

getDescription :: Solver -> Text
getDescription (StarExecSolver s) = solverInfoDescription s
getDescription (LriSolver s) = lriSolverInfoDescription s
getDescription _ = notAvailable

getLastUpdate :: Solver -> Text
getLastUpdate (StarExecSolver s) = fromString $ show $ solverInfoLastUpdate s
getLastUpdate _ = notAvailable

getAuthor :: Solver -> Text
getAuthor (LriSolver s) = lriSolverInfoAuthor s
getAuthor _ = notAvailable

getUrl :: Solver -> Text
getUrl (LriSolver s) = lriSolverInfoUrl s
getUrl _ = notAvailable

isStandard :: Solver -> Text
isStandard (LriSolver s) = fromBool $ lriSolverInfoIsStandard s
isStandard _ = no

isConditional :: Solver -> Text
isConditional (LriSolver s) = fromBool $ lriSolverInfoIsConditional s
isConditional _ = no

isContextSensitive :: Solver -> Text
isContextSensitive (LriSolver s) = fromBool $ lriSolverInfoIsContextSensitive s
isContextSensitive _ = no

isInnermost :: Solver -> Text
isInnermost (LriSolver s) = fromBool $ lriSolverInfoIsInnermost s
isInnermost _ = no

isTheory :: Solver -> Text
isTheory (LriSolver s) = fromBool $ lriSolverInfoIsTheory s
isTheory _ = no

isCertifying :: Solver -> Text
isCertifying (LriSolver s) = fromBool $ lriSolverInfoIsCertifying s
isCertifying _ = no

getShowSolverInfoR :: SolverID -> Handler Html
getShowSolverInfoR sid@(StarExecSolverID _id) = do
  (QueryResult qStatus mSolverInfo) <- querySolverInfo sid
  defaultLayout $ do
    case qStatus of
      Latest -> return ()
      Pending _ -> insertWidgetMetaRefresh
    $(widgetFile "se_show_solver_info")
getShowSolverInfoR sid@(LriSolverID _id) = do
  mSolverInfo <- getPersistSolverInfo sid
  defaultLayout $ do
    $(widgetFile "lri_show_solver_info")
