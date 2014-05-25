module Handler.FindPrim where

import Import
import qualified Data.Text as T
import StarExec.Types
import StarExec.Connection
import StarExec.Prims

parsePrimId :: Text -> Int
parsePrimId = read . T.unpack

parsePrimType :: Text -> StarExecListType
parsePrimType "Jobs" = Jobs
parsePrimType "Spaces" = Spaces
parsePrimType "Solvers" = Solvers
parsePrimType "Benchmarks" = Benchmarks
parsePrimType "Users" = Users

getFindPrimR :: Handler Html
getFindPrimR = do
    defaultLayout $ do
        let mPrimInfo = Nothing :: Maybe PrimInfo
        $(widgetFile "find_prim")

postFindPrimR :: Handler Html
postFindPrimR = do
    (tPrimType, tPrimId) <- runInputPost $ (,) <$> ireq textField "primType"
                                               <*> ireq textField "id"
    let primType = parsePrimType tPrimType
        primId = parsePrimId tPrimId
    con <- getConnection
    mPrimInfo <- searchPrim con primId primType
    defaultLayout $ do
        $(widgetFile "find_prim")

