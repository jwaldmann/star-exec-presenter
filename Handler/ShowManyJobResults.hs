{-# language StandaloneDeriving #-}
{-# language FlexibleInstances #-}

module Handler.ShowManyJobResults
  ( getShowManyJobResultsR
  -- , getShowManyJobResultsLegacyR
  , shorten
  ) where

import Import
import Presenter.Short
import Presenter.StarExec.JobData
import Presenter.Internal.Stringish()
import Presenter.Processing
import Presenter.Statistics
import Presenter.Utils.WidgetMetaRefresh
import Presenter.Utils.WidgetTable
import qualified Presenter.DOI as DOI

import Text.Lucius (luciusFile)
import Data.Double.Conversion.Text
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Text as T

toTuples :: (a, [b]) -> [(a,b)]
toTuples (i, solvers) = map ((,) i) solvers

shorten :: Text -> Text
shorten t = T.takeEnd 50 t

getShowManyJobResultsR
  :: Scoring -> Query -> JobIds -> Handler Html
getShowManyJobResultsR sc NoQuery  jids@(JobIds ids) = do

  dois <- doiService <$> getYesod
  
  qJobs <- queryManyJobs ids

  let jobInfos = catMaybes $ map (fst . queryResult) qJobs
      -- complexity = all isComplexity jobInfos
      complexity = (sc == Complexity)
      jobs = map (scoredResults sc . snd . queryResult) qJobs

      jobResults :: [JobResult]
      jobResults = concat $ jobs

      stat = mconcat $ map jobStat jobResults


      benchmarks' = L.sortBy compareBenchmarks $
                      getInfo extractBenchmark $ jobResults

      groupedSolvers = map (getInfo extractSolver) jobs
      jobSolvers = concat $ map toTuples $ zip ids groupedSolvers
      -- FIXME: this doubles work (we already have the results)
      benchmarkResults_ = getBenchmarkResults
                          jobSolvers
                          jobResults
                          benchmarks'

      -- alternative implementation:
      resultmap
        :: M.Map
           UniqueBenchmark --  <== this is wrong, should be BenchmarkKey
           (M.Map (JobID, (SolverID, SolverName)) JobResult)
      resultmap = M.fromListWith M.union $ do
        jr <- jobResults
        return ( bminfo dois $ getBenchmark jr
               , M.singleton (getJobID jr, getSolver jr) jr
               )
      benchmarkResults = do
        (bm, rowmap) <- M.toList resultmap
        let row = do
              s <- jobSolvers
              return $ M.lookup s rowmap
        return (bm, row)
      scores = for jobs $  \ results ->
        calculateScores sc results
  defaultLayout $ do
    toWidget $(luciusFile "templates/solver_result.lucius")
    if any (\q -> case queryStatus q of Latest -> False ; _ -> True) qJobs
      then insertWidgetMetaRefresh
      else return ()
    $(widgetFile "show_many_job_results")

getShowManyJobResultsR sc q@(Query ts) jids @ (JobIds ids) = do
  qJobs <- queryManyJobs ids
  tab <- getManyJobCells $ map (snd . queryResult) qJobs
  defaultLayout $ do
    setTitle "Flexible Table"
    toWidget $(luciusFile "templates/solver_result.lucius")
    if any (\q' -> queryStatus q' /= Latest) qJobs
      then insertWidgetMetaRefresh
      else return ()
    [whamlet|
            <pre>#{show q}
        |]
    display sc jids [] ts tab
