module Handler.ShowSolverInfo where

import Import
import Presenter.StarExec.JobData
import Presenter.Internal.Stringish
import Presenter.Utils.WidgetMetaRefresh

getDescription :: Solver -> Text
getDescription (StarExecSolver s) = solverInfoDescription s
getDescription _ = ""

getLastUpdate :: Solver -> Text
getLastUpdate (StarExecSolver s) = fromString $ show $ solverInfoLastUpdate s
getLastUpdate _ = ""

getShowSolverInfoR :: SolverID -> Handler Html
getShowSolverInfoR sid@(StarExecSolverID _id) = do
  (QueryResult qStatus mSolverInfo) <- querySolverInfo sid
  defaultLayout $ do
    case qStatus of
      Latest -> return ()
      Pending _ -> insertWidgetMetaRefresh
    $(widgetFile "se_show_solver_info")
