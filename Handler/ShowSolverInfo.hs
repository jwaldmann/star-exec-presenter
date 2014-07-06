module Handler.ShowSolverInfo where

import Import
import StarExec.JobData
import StarExec.Types

getShowSolverInfoR :: Int -> Handler Html
getShowSolverInfoR _solverId = do
  (QueryResult qStatus mSolverInfo) <- querySolverInfo _solverId
  defaultLayout $ do
    $(widgetFile "show_solver_info")
