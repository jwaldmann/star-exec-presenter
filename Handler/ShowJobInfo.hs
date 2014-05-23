module Handler.ShowJobInfo where

import Import
import qualified StarExec.Commands as SEC
import StarExec.Types
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

updateDB jobId results = do
    mapM (\sr -> do
            let pId = jriPairId sr
            _ <- runDB $ do
                mSolverResult <- getBy $ UniquePersistJobResultInfo pId
                case mSolverResult of
                    Just solverResult -> return Nothing
                    Nothing -> do
                        jobResultId <- insertUnique $ PersistJobResultInfo
                            jobId
                            (jriPairId sr)
                            (jriBenchmark sr)
                            (jriBenchmarkId sr)
                            (jriSolver sr)
                            (jriSolverId sr)
                            (jriConfiguration sr)
                            (jriConfigurationId sr)
                            (jriStatus sr)
                            (jriCpuTime sr)
                            (jriWallclockTime sr)
                            (jriResult sr)
                        return Nothing
            return jriResult
        ) results
    _ <- runDB $ insertUnique $ PersistJobInfo jobId Complete
    return results

getResultsFromStarExec :: ( MonadHandler m, MonadBaseControl IO m )
    => Int -> m [JobResultInfo]
getResultsFromStarExec jobId = do
    con <- SEC.getConnection
    mInfo <- SEC.getJobInfo con jobId
    return $ case mInfo of
        Just info -> info
        Nothing   -> []

toJobResultInfo results = map (\entity ->
    let jobResult = entityVal entity
    in JobResultInfo
        { jriPairId = persistJobResultInfoPairId jobResult
        , jriBenchmark = persistJobResultInfoBenchmark jobResult
        , jriBenchmarkId = persistJobResultInfoBenchmarkId jobResult
        , jriSolver = persistJobResultInfoSolver jobResult
        , jriSolverId = persistJobResultInfoSolverId jobResult
        , jriConfiguration = persistJobResultInfoConfiguration jobResult
        , jriConfigurationId = persistJobResultInfoConfigurationId jobResult
        , jriStatus = persistJobResultInfoStatus jobResult
        , jriCpuTime = persistJobResultInfoCpuTime jobResult
        , jriWallclockTime = persistJobResultInfoWallclockTime jobResult
        , jriResult = persistJobResultInfoResult jobResult
        }
    ) results

getShowJobInfoR :: Int -> Handler Html
getShowJobInfoR jobId = do
    --mJobId <- runDB $ getBy $ UniqueJob jobId
    --jobinfos <- case mJobId of
    --    Just jid -> runDB $ do
    --        results <- selectList [ JobResultStarExecJobId ==. jobId ] []
    --        toJobResultInfo results
    --    Nothing -> do
    jobinfos <- getResultsFromStarExec jobId
            --updateDB jobId infos
    defaultLayout $ do
        $(widgetFile "show_job_info")
