module StarExec.CompetitionResults 
  ( getCompetitionResults
  ) where

import Import
import StarExec.Types
import StarExec.Connection
import StarExec.JobData
import StarExec.Persist

getCategoriesResult cat = do
  let catFilter = getCategoryFilter cat
      catJobIds = getJobIds cat
  pResults <- getManyJobResults catJobIds
  let results = concat $ map fromPersistJobResultInfos pResults
      solver = getInfo extractSolver results
      --scores = 
  return undefined

getMetaResults metaCat = do
  catResults <- mapM getCategoriesResult $ getCategories metaCat
  return undefined

getCompetitionResults comp = do
  con <- getConnection
  metaResults <- mapM getMetaResults $ getMetaCategories comp
  return undefined
