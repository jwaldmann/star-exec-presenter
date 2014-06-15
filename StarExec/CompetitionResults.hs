module StarExec.CompetitionResults 
  ( getCompetitionResults
  ) where

import Import
import StarExec.Types
import StarExec.Connection
import StarExec.JobData
import StarExec.Persist

getCategoriesResult con cat = do
  let catFilter = getCategoryFilter cat
      catJobIds = getJobIds cat
  pResults <- getManyJobResultsWithConnection con catJobIds
  let results = concat $ map fromPersistJobResultInfos pResults
      solver = getInfo extractSolver results
      --scores = 
  return undefined

getMetaResults con metaCat = do
  catResults <- mapM (getCategoriesResult con) $ getCategories metaCat
  return undefined

getCompetitionResults comp = do
  con <- getConnection
  metaResults <- mapM (getMetaResults con) $ getMetaCategories comp
  return undefined
