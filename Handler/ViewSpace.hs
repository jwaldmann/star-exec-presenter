module Handler.ViewSpace where

import Import
import qualified StarExec.Commands as SEC
import StarExec.Types
import qualified Data.Text as T

data StarExecSpace = StarExecSpace { sId :: Int
                                   , sName :: Text
                                   }

getLinkList :: [(Text, Text)] -> [StarExecSpace]
getLinkList spaces = map (\(name, sid) ->
        StarExecSpace { sId = read $ T.unpack sid
                      , sName = name
                      }
    ) spaces

getViewSpaceR :: Int -> Handler Html
getViewSpaceR spaceId = do
    con <- SEC.getConnection
    spaceContent <- SEC.listPrim con spaceId Spaces 2
    defaultLayout $ do
        let subspaces =
                case spaceContent of
                    Just spaces -> getLinkList spaces
                    Nothing -> []
        $(widgetFile "view_space")
