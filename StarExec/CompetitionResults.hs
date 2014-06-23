module StarExec.CompetitionResults 
  ( getCompetitionResults
  , CompetitionResults(..)
  , MetaCategoryResult(..)
  , CategoryResult(..)
  ) where

import Import
import StarExec.Types
import StarExec.JobData
import qualified Data.List as L
import Data.Maybe

data CompetitionResults = CompetitionResults
  { competitionName :: Name
  , metaCategoryResults :: [MetaCategoryResult] 
  } deriving (Show)

data MetaCategoryResult = MetaCategoryResult
  { metaCategoryName :: Name
  , categoryResults :: [CategoryResult]
  , metaCategoryRanking :: [(Maybe Rank, Solver, Score)]
  } deriving (Show)

data CategoryResult = CategoryResult
  { categoryName :: Name
  , categoryRanking :: [(Maybe Rank, Solver, Score)]
  , categoryJobs :: [JobInfo]
  } deriving (Show)

{-
  TODOs:
    for all categories: show jobIds and their status
      -> mark jobs Complete/Incomplete
    same results -> special case
-}

sortScore :: (Solver, Score) -> (Solver, Score) -> Ordering
sortScore (_,i1) (_,i2) = compare i1 i2

getScores :: [Solver] -> [SolverResult] -> [JobResultInfo] -> [(Solver,Score)]
getScores solver filters results =
  reverse $ L.sortBy sortScore $ map countResults solver
  where
    countResults s = (s, length $ filter (matches s) results)
    matches (sid,sn) jri = any (\f ->
        jobResultInfoSolverId jri == sid &&
        jobResultInfoSolver jri == sn &&
        jobResultInfoResult jri == f
      ) filters

getRanking :: [(Solver, Score)] -> [(Maybe Rank, Solver, Score)]
getRanking scores =
  let indexedScores = zip [1..] scores :: [(Rank, (Solver, Score))]
      equals score (_,(_,scr)) = score == scr
      getRanking' :: (Solver, Score) -> (Maybe Rank, Solver, Score)
      getRanking' (solver, score) =
          case filter (equals score) indexedScores of
            [] -> (Nothing, solver, score)
            ((rank,(slv,_)):_) ->
              (if solver /= slv then Nothing else Just rank, solver, score)
  in map getRanking' scores

getCategoriesResult :: Category -> Handler CategoryResult
getCategoriesResult cat = do
  let catName = getCategoryName cat
      catFilter = getCategoryFilter cat
      catJobIds = getJobIds cat
  pResults <- getManyJobResults catJobIds
  mJobInfos <- mapM getJobInfo catJobIds
  let results = concat $ pResults
      solver = getInfo extractSolver results
      scores = getScores solver catFilter results
      rankedSolver = getRanking scores
      jobInfos = catMaybes mJobInfos
  return $ CategoryResult catName
                          rankedSolver
                          jobInfos

calcScores :: [(Maybe Rank, Solver, Score)] -> [(Solver, Score)]
calcScores [(_, slv, _)] = [(slv, 0)]
calcScores ranking = calcScores' 1 ranking
  where
    numRankings = length ranking
    calcScores' rank ((mRank,slv,_):rs) =
      case mRank of
        Nothing -> (slv,numRankings - rank):calcScores' rank rs
        Just r -> (slv,numRankings - r):calcScores' r rs
    calcScores' _ [] = []

getMetaScores :: [[(Maybe Rank, Solver, Score)]] -> [(Solver, Score)]
getMetaScores catScores =
  let catScores' = concat $ map calcScores catScores
      solvers = L.nub $ map fst catScores'
      countScore s = (s, L.foldl' (\i (slv,scr) ->
          if slv == s then i + scr else scr
        ) 0 catScores')
  in map countScore solvers

getMetaResults :: MetaCategory -> Handler MetaCategoryResult
getMetaResults metaCat = do
  let metaName = getMetaCategoryName metaCat
      categories = getCategories metaCat
  catResults <- mapM getCategoriesResult categories
  let scores = getMetaScores $ map categoryRanking catResults
      ranking = getRanking $ reverse $ L.sortBy sortScore scores
  return $ MetaCategoryResult metaName
                              catResults
                              ranking

getCompetitionResults :: Competition -> Handler CompetitionResults
getCompetitionResults comp = do
  let compName = getCompetitionName comp
      metaCats = getMetaCategories comp
  metaResults <- mapM getMetaResults metaCats
  return $ CompetitionResults compName
                              metaResults
