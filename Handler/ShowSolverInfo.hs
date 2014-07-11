module Handler.ShowSolverInfo where

import Import
import StarExec.JobData
import StarExec.Types
import Utils.WidgetMetaRefresh

getShowSolverInfoR :: Int -> Handler Html
getShowSolverInfoR _solverId = do
  (QueryResult qStatus mSolverInfo) <- querySolverInfo _solverId
  defaultLayout $ do
    case qStatus of
      Latest -> return ()
      Pending _ -> insertWidgetMetaRefresh
    $(widgetFile "show_solver_info")
