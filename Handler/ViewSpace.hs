module Handler.ViewSpace where

import Import
import StarExec.Prims
import StarExec.Connection
import StarExec.Types
import StarExec.Session

getViewSpaceR :: Int -> Handler Html
getViewSpaceR _spaceId = do
    loggedIn <- hasValidSession
    if not loggedIn
        then redirect HomeR
        else do
            con <- getConnection
            spaceList <- listPrim con _spaceId Spaces
            jobList <- listPrim con _spaceId Jobs

            defaultLayout $ do
                let subspaces = eitherToSpaceInfos spaceList
                    jobs = eitherToJobInfos jobList
                $(widgetFile "view_space")
