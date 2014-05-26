module Handler.SearchPrim where

import Import
import qualified Data.Text as T
import StarExec.Types
import StarExec.Connection
import StarExec.Prims
import StarExec.Session

parsePrimId :: Text -> Int
parsePrimId = read . T.unpack

parsePrimType :: Text -> StarExecListType
parsePrimType "Jobs" = Jobs
parsePrimType "Spaces" = Spaces
parsePrimType "Solvers" = Solvers
parsePrimType "Benchmarks" = Benchmarks
parsePrimType "Users" = Users

getSearchPrimR :: Handler Html
getSearchPrimR = do
    loggedIn <- hasValidSession
    if not loggedIn
        then redirect HomeR
        else do
            defaultLayout $ do
                let mPrimInfo = Nothing :: Maybe PrimInfo
                $(widgetFile "search_prim")

postSearchPrimR :: Handler Html
postSearchPrimR = do
    loggedIn <- hasValidSession
    if not loggedIn
        then redirect HomeR
        else do
            (tPrimType, tPrimId) <- runInputPost $ (,) <$> ireq textField "primType"
                                                       <*> ireq textField "id"
            let primType = parsePrimType tPrimType
                primId = parsePrimId tPrimId
            con <- getConnection
            mPrimInfo <- searchPrim con primId primType
            defaultLayout $ do
                $(widgetFile "search_prim")

