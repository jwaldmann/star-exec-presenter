module Handler.ViewSpace where

import Import
import qualified StarExec.Commands as SEC
import StarExec.Types

getViewSpaceR :: Int -> Handler Html
getViewSpaceR spaceId = defaultLayout $ do
    con <- SEC.getConnection
    spaceContent <- SEC.listPrim con spaceId Spaces 2
    $(widgetFile "view_space")
