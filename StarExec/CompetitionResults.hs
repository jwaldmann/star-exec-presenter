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

getScores :: [Solver] -> [SolverResult] -> [JobResultInfo] -> [(Solver,Score)]
getScores solver filters results =
  reverse $ L.sortBy sortScore $ map countResults solver
  where
    sortScore (_,i1) (_,i2) = compare i1 i2
    rawResults = map jriResult results
    countResults s = (s, length $ filter matches rawResults)
    matches result = any (==result) filters

getRanking :: [(Solver, Score)] -> [(Maybe Rank, Solver, Score)]
getRanking scores =
  let indexedScores = zip [1..] scores :: [(Rank, (Solver, Score))]
      equals score (_,(_,scr)) = score == scr
      getRanking' :: (Solver, Score) -> (Maybe Rank, Solver, Score)
      getRanking' (solver, score) =
          case filter (equals score) indexedScores of
            [] -> (Nothing, solver, score)
            ((rank,(slv,scr)):_) ->
              (if solver /= slv then Nothing else Just rank, solver, score)
  in map getRanking' scores



  --map getRanking' $ zipWith [1..] scores
  --where getRanking' (solv, score)

getCategoriesResult :: Category -> Handler CategoryResult
getCategoriesResult cat = do
  let catName = getCategoryName cat
      catFilter = getCategoryFilter cat
      catJobIds = getJobIds cat
  pResults <- getManyJobResults catJobIds
  mJobInfos <- mapM getJobInfo catJobIds
  let results = concat $ map fromPersistJobResultInfos pResults
      solver = getInfo extractSolver results
      scores = getScores solver catFilter results
      rankedSolver = getRanking scores
      jobInfos = catMaybes mJobInfos
  return $ CategoryResult
            { categoryName = catName
            , categorySolver = rankedSolver
            , categoryJobs = jobInfos
            }

getMetaResults :: MetaCategory -> Handler MetaCategoryResult
getMetaResults metaCat = do
  let metaName = getMetaCategoryName metaCat
      categories = getCategories metaCat
  catResults <- mapM getCategoriesResult categories
  return $ MetaCategoryResult
            { metaCategoryName = metaName
            , categoryResults = catResults
            }

getCompetitionResults :: Competition -> Handler CompetitionResults
getCompetitionResults comp = do
  let compName = getCompetitionName comp
      metaCats = getMetaCategories comp
  metaResults <- mapM getMetaResults metaCats
  return $ CompetitionResults
            { competitionName = compName
            , metaCategoryResults = metaResults
            }
