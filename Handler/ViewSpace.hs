module Handler.ViewSpace where

import Import
import qualified StarExec.Commands as SEC
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
getViewSpaceR spaceId = do
    con <- SEC.getConnection
    spaceList <- SEC.listPrim con spaceId Spaces 2
    jobList <- SEC.listPrim con spaceId Jobs 6

    defaultLayout $ do
        let subspaces =
                case spaceList of
                    Just spaces -> getPrimList spaces
                    Nothing -> []
            jobs =
                case jobList of
                    Just js -> getPrimList js
                    Nothing -> []
        $(widgetFile "view_space")
