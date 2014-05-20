module Handler.ShowJobInfo where

import Import
import qualified StarExec.Commands as SEC
import StarExec.JobResultInfo
import StarExec.SolverResult
import StarExec.JobStatus
import Data.Double.Conversion.Text

getClass :: JobResultInfo -> Text
getClass info =
    case result info of
        YES    -> "success"
        NO     -> "success"
        MAYBE  -> "success"
        ERROR  -> "danger"
        _      -> "warning"

updateDB jobId results = do
    mapM (\sr -> do
            let pId = pairId sr
            _ <- runDB $ do
                mSolverResult <- getBy $ UniquePersistJobResultInfo pId
                case mSolverResult of
                    Just solverResult -> return Nothing
                    Nothing -> do
                        jobResultId <- insertUnique $ PersistJobResultInfo
                            jobId
                            (pairId sr)
                            (benchmark sr)
                            (benchmarkId sr)
                            (solver sr)
                            (solverId sr)
                            (configuration sr)
                            (configurationId sr)
                            (status sr)
                            (cpuTime sr)
                            (wallclockTime sr)
                            (result sr)
                        return Nothing
            return result
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
        { pairId = persistJobResultInfoPairId jobResult
        , benchmark = persistJobResultInfoBenchmark jobResult
        , benchmarkId = persistJobResultInfoBenchmarkId jobResult
        , solver = persistJobResultInfoSolver jobResult
        , solverId = persistJobResultInfoSolverId jobResult
        , configuration = persistJobResultInfoConfiguration jobResult
        , configurationId = persistJobResultInfoConfigurationId jobResult
        , status = persistJobResultInfoStatus jobResult
        , cpuTime = persistJobResultInfoCpuTime jobResult
        , wallclockTime = persistJobResultInfoWallclockTime jobResult
        , result = persistJobResultInfoResult jobResult
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
