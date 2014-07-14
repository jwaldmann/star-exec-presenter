module Handler.ShowManyJobResults
  ( getShowManyJobResultsR
  , getShowManyJobResultsLegacyR
  ) where

import Import
import StarExec.Types
import StarExec.Persist
import StarExec.JobData
import StarExec.Processing
import Data.Double.Conversion.Text
import Data.Maybe
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.IntMap.Strict as IM
import Text.Lucius (luciusFile)
import Table.Query
import Utils.WidgetMetaRefresh
--import Database.Persist.Sql
--import Control.Exception.Lifted (catch)
--import Data.Ratio
--import Control.Monad (void)

countResults :: SolverResult -> (Int, (Int, Text)) -> Handler Int
countResults result (_jobId,(sid,_)) = runDB $ do
  count [ JobResultInfoJobId ==. _jobId
        , JobResultInfoSolverId ==. sid
        , JobResultInfoResult ==. result
        ]

countResultsYes :: (Int, (Int, Text)) -> Handler Int
countResultsYes (_jobId,(sid,_)) = runDB $ do
  count [ JobResultInfoJobId ==. _jobId
        , JobResultInfoSolverId ==. sid
        , JobResultInfoResult !=. NO
        , JobResultInfoResult !=. MAYBE
        , JobResultInfoResult !=. CERTIFIED
        , JobResultInfoResult !=. ERROR
        , JobResultInfoResult !=. OTHER
        ]

toTuples :: (a, [b]) -> [(a,b)]
toTuples (i, solvers) = map ((,) i) solvers

shorten :: Text -> Text
shorten t = if T.length t > 50
              then shorten $ T.tail t
              else t

calcScore :: [Int] -> Int -> Handler Int
calcScore [] _solverId = return 0
calcScore _jobIds _solverId = do
  let solverFilter = JobResultInfoSolverId ==. _solverId
      eqFilters = map (\i -> [JobResultInfoJobId ==. i, solverFilter]) _jobIds
      jobFilter = L.foldr1 (||.) $ eqFilters
  solverResults <- runDB $ selectList jobFilter []
  return $ sum $ catMaybes $ map (jobResultInfoScore . entityVal) solverResults

--calcScore :: Int -> Handler [Int]
--calcScore _solverId = do
--  let sql = "SELECT sum(score) FROM job_result_info WHERE solver_id = ?;"
--  scores <- catch
--    ( runDB $ rawSql sql [ toPersistValue _solverId ] )
--    ( return [ Single $ 0 % 1 ] )
--  case scores of
--    [] -> return [0]
--    xs -> return $ map (truncate . fromRational . unSingle) xs

--  | to keep old URLs working, as in
--  http://lists.lri.fr/pipermail/termtools/2014-July/000965.html
getShowManyJobResultsLegacyR :: JobIds -> Handler Html
getShowManyJobResultsLegacyR = getShowManyJobResultsR

getShowManyJobResultsR :: JobIds -> Handler Html
getShowManyJobResultsR jids @ (JobIds ids) = do
  qJobs <- queryManyJobs ids
  let jobInfos = catMaybes $ map (fst . queryResult) qJobs
      isComplexity = all jobInfoIsComplexity jobInfos
      jobs = map (snd . queryResult) qJobs
      jobResults = concat $ jobs
      benchmarks = L.sortBy compareBenchmarks $
                    getInfo extractBenchmark $ jobResults
      groupedSolvers = map (getInfo extractSolver) jobs
      jobSolvers = concat $ map toTuples $ zip ids groupedSolvers
      solvers = concat $ groupedSolvers
      benchmarkResults = getBenchmarkResults
                          solvers
                          jobResults
                          benchmarks
      (+>) = T.append
      results = [ "YES", "NO", "MAYBE", "CERTIFIED", "ERROR", "OTHER" ] :: [T.Text]
  yesses <- mapM countResultsYes jobSolvers
  nos    <- mapM (countResults NO) jobSolvers
  maybes <- mapM (countResults MAYBE) jobSolvers
  certs  <- mapM (countResults CERTIFIED) jobSolvers
  errors <- mapM (countResults ERROR) jobSolvers
  others <- mapM (countResults OTHER) jobSolvers
  let scores = flip map jobs $
        \results ->
          if isComplexity
            then calcComplexityScores results
            else calcStandardScores results
  defaultLayout $ do
    toWidget $(luciusFile "templates/solver_result.lucius")
    if any (\q -> queryStatus q /= Latest) qJobs
      then insertWidgetMetaRefresh
      else return ()
    $(widgetFile "show_many_job_results")
