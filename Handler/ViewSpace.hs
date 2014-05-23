module Handler.ViewSpace where

import Import
import StarExec.Prims
import StarExec.Connection
import StarExec.Types
import qualified Data.Text as T

data StarExecPrim = StarExecPrim { sId :: Int
                                 , sName :: Text
                                 }

getPrimList :: [(Text, Text)] -> [StarExecPrim]
getPrimList prims = map (\(name, sid) ->
        StarExecPrim { sId = read $ T.unpack sid
                     , sName = name
                     }
    ) prims

getViewSpaceR :: Int -> Handler Html
getViewSpaceR sId = do
    con <- getConnection
    spaceList <- listPrim con sId Spaces
    jobList <- listPrim con sId Jobs

    defaultLayout $ do
        let subspaces = map toSpaceInfo $ fromEither spaceList
            jobs = map toJobInfo $ fromEither jobList
        $(widgetFile "view_space")
