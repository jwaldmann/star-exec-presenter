module Handler.ViewSpace where

import Import
import StarExec.Prims
import StarExec.Connection
import StarExec.Types

getViewSpaceR :: Int -> Handler Html
getViewSpaceR _spaceId = do
  liftIO $ putStrLn "getting connection:"
  con <- getConnection
  liftIO $ putStrLn "getting spaceList"
  spaceList <- listPrim con _spaceId Spaces
  liftIO $ putStrLn "getting jobList"
  jobList <- listPrim con _spaceId Jobs

  defaultLayout $ do
      let subspaces = eitherToSpaceInfos spaceList
          jobs = eitherToJobInfos jobList
      $(widgetFile "view_space")
