module Handler.ListSolvers where

import Import
import Presenter.PersistHelper

getAll :: Handler [Solver]
getAll = runDB $ do
  starExecSolvers <- do
    solvers <- getEntityList' ([] :: [Filter SolverInfo]) []
    return $ StarExecSolver <$> solvers
  lriSolvers <- do
    solvers <- getEntityList' ([] :: [Filter LriSolverInfo]) []
    return $ LriSolver <$> solvers
  return $ starExecSolvers ++ lriSolvers

getListSolversR :: Handler Html
getListSolversR = do
  solvers <- getAll
  defaultLayout $ do
    $(widgetFile "list_solvers")
