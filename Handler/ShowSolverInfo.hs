module Handler.ShowSolverInfo where

import Import
import StarExec.Connection
import StarExec.Commands

getShowSolverInfoR :: Int -> Handler Html
getShowSolverInfoR _solverId = do
  con <- getConnection
  mSolverInfo <- getSolverInfo con _solverId
  defaultLayout $ do
    $(widgetFile "show_solver_info")
