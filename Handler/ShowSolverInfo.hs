module Handler.ShowSolverInfo where

import Import
import Presenter.StarExec.JobData
import Presenter.Utils.WidgetMetaRefresh

getShowSolverInfoR :: SolverID -> Handler Html
getShowSolverInfoR (StarExecSolverID _id) = do
  (QueryResult qStatus mSolverInfo) <- querySolverInfo _id
  defaultLayout $ do
    case qStatus of
      Latest -> return ()
      Pending _ -> insertWidgetMetaRefresh
    $(widgetFile "se_show_solver_info")
