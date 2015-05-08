{-# language StandaloneDeriving #-}
{-# language FlexibleInstances #-}

module Handler.ShowManyJobResults
  ( getShowManyJobResultsR
  , getShowManyJobResultsLegacyR
  ) where

import Import
import StarExec.Types
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
import StarExec.Statistics

toTuples :: (a, [b]) -> [(a,b)]
toTuples (i, solvers) = map ((,) i) solvers

shorten :: Text -> Text
shorten t = if T.length t > 50
              then shorten $ T.tail t
              else t

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

      jobResults :: [JobResultInfo]
      jobResults = concat $ jobs

      stat = mconcat $ map jobStat jobResults

      benchmarks = L.sortBy compareBenchmarks $
                    getInfo extractBenchmark $ jobResults
      groupedSolvers = map (getInfo extractSolver) jobs
      jobSolvers = concat $ map toTuples $ zip ids groupedSolvers
      benchmarkResults = getBenchmarkResults
                          jobSolvers
                          jobResults
                          benchmarks
      (+>) = T.append
      scores = flip map jobs $
        \results ->
          if isComplexity
            then calcComplexityScores results
            else calcStandardScores results
  defaultLayout $ do
    toWidget $(luciusFile "templates/solver_result.lucius")
    if any (\q -> case queryStatus q of Latest -> False ; _ -> True) qJobs
      then insertWidgetMetaRefresh
      else return ()
    $(widgetFile "show_many_job_results")
