module Handler.CompareJobs where

import Import
import StarExec.Session
import StarExec.Commands
import StarExec.Types
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.List as L

type JobResultInfos = [JobResultInfo]
type BenchmarkID = Int
type JobID = Int
type SolverID = Int
type SolverResults = [SolverResult]
type JobSolverCombi = (JobID, SolverID)
type BenchmarkCell = (BenchmarkID, SolverResults)
type TableHead = [JobSolverCombi]

getResultsFromStarExec :: ( MonadHandler m, MonadBaseControl IO m )
    => Int -> m JobResultInfos
getResultsFromStarExec _jobId = do
    con <- getConnection
    mInfo <- getJobInfo con _jobId
    return $ case mInfo of
        Just info -> info
        Nothing   -> []

parseIds :: T.Text -> [Int]
--parseIds = map (read . show) . T.words
parseIds ids = map (read . T.unpack) $ T.words ids

getSolverIds :: JobResultInfos -> [Int]
getSolverIds = S.toList . L.foldr (\jri set -> S.insert (jriSolverId jri) set) S.empty

getBenchmarkIds :: JobResultInfos -> [Int]
getBenchmarkIds = S.toList . L.foldr (\jri set -> S.insert (jriBenchmarkId jri) set) S.empty

getCompareJobsR :: Handler Html
getCompareJobsR = do
    loggedIn <- hasValidSession
    if not loggedIn
        then redirect HomeR
        else do
            defaultLayout $ do
                let mTable = Nothing
                $(widgetFile "compare_jobs")

postCompareJobsR :: Handler Html
postCompareJobsR = do
  ids <- runInputPost $ ireq textField "ids"
  let idList = parseIds ids
  jobs <- mapM getResultsFromStarExec idList
  let bencharkIds = S.toList $ S.fromList $
          L.foldr (++) [] $ map getBenchmarkIds jobs
      solverIds = map getSolverIds jobs

  defaultLayout $ do
      --let mTable = Just $ zip ids jobs
      $(widgetFile "compare_jobs")
