module StarExec.CompetitionResults 
  ( getCompetitionResults
  ) where

import Import
import StarExec.Types
import StarExec.Connection
import StarExec.JobData
import StarExec.Persist
import qualified Data.List as L
import Data.Maybe

{-
  TODOs:
    for all categories: show jobIds and their status
      -> mark jobs Complete/Incomplete
    same results -> special case
-}

getScores :: [Solver] -> [SolverResult] -> [JobResultInfo] -> [(Solver,Int)]
getScores solver filters results =
  reverse $ L.sortBy sortScore $ map countResults solver
  where
    sortScore (_,i1) (_,i2) = compare i1 i2
    rawResults = map jriResult results
    countResults s = (s, length $ filter matches rawResults)
    matches result = any (==result) filters

getCategoriesResult cat = do
  let catFilter = getCategoryFilter cat
      catJobIds = getJobIds cat
  pResults <- getManyJobResults catJobIds
  mJobInfos <- mapM getJobInfo catJobIds
  let results = concat $ map fromPersistJobResultInfos pResults
      solver = getInfo extractSolver results
      scores = getScores solver catFilter results
      jobInfos = catMaybes mJobInfos
  return (jobInfos, scores)

getMetaResults metaCat = do
  catResults <- mapM getCategoriesResult $ getCategories metaCat
  return catResults

getCompetitionResults comp = do
  metaResults <- mapM getMetaResults $ getMetaCategories comp
  return metaResults
