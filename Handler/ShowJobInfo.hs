module Handler.ShowJobInfo where

import Import
import qualified StarExec.Commands as SEC
import StarExec.Types
import StarExec.Session
import Data.Double.Conversion.Text

getClass :: JobResultInfo -> Text
getClass info =
    case jriResult info of
        YES       -> "success"
        NO        -> "success"
        MAYBE     -> "success"
        CERTIFIED -> "success"
        ERROR     -> "danger"
        _         -> "warning"

getResultsFromStarExec :: ( MonadHandler m, MonadBaseControl IO m )
    => Int -> m [JobResultInfo]
getResultsFromStarExec _jobId = do
    con <- SEC.getConnection
    mInfo <- SEC.getJobInfo con _jobId
    return $ case mInfo of
        Just info -> info
        Nothing   -> []

getShowJobInfoR :: Int -> Handler Html
getShowJobInfoR _jobId = do
    loggedIn <- hasValidSession
    if not loggedIn
        then redirect HomeR
        else do
            --mJobId <- runDB $ getBy $ UniqueJob jobId
            --jobinfos <- case mJobId of
            --    Just jid -> runDB $ do
            --        results <- selectList [ JobResultStarExecJobId ==. jobId ] []
            --        toJobResultInfo results
            --    Nothing -> do
            jobinfos <- getResultsFromStarExec _jobId
                    --updateDB jobId infos
            defaultLayout $ do
                $(widgetFile "show_job_info")
