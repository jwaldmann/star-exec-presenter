-- | semantics specification:
-- In each meta-category, a medal will be awarded to the highest-scoring solver.
-- For every meta-category, we consider the sum of the scores 
-- for each category within that meta-category: 
-- The score of a tool is determined by the number of other tools
-- which could be beaten in that category. 
-- http://www.termination-portal.org/wiki/Termination_Competition_2014#Competition_Categories_and_Awards

module StarExec.CompetitionResults 
  ( getCompetitionResults
  , CompetitionResults(..)
  , MetaCategoryResult(..)
  , CategoryResult(..)
  ) where

import Import
import StarExec.Types
import StarExec.JobData
import StarExec.Processing
import qualified Data.List as L
import Data.Maybe
import Data.Time.Clock
import qualified Data.IntMap.Strict as IM


import qualified Data.Map as M

data CompetitionResults = CompetitionResults
  { competitionName :: Name
  , metaCategoryResults :: [MetaCategoryResult]
  , competitionComplete :: Bool 
  , competitionStartTime :: Maybe UTCTime
  , competitionFinishTime :: Maybe UTCTime
  } deriving (Show)

data MetaCategoryResult = MetaCategoryResult
  { metaCategoryName :: Name
  , categoryResults :: [CategoryResult]
  , metaCategoryRanking :: [(Maybe Rank, Solver, Score)]
  , metaCategoryComplete :: Bool
  , metaCategoryStarTime :: Maybe UTCTime
  , metaCategoryFinishTime :: Maybe UTCTime
  } deriving (Show)

data CategoryResult = CategoryResult
  { categoryName :: Name
  , categoryScoring :: Scoring
  , categoryPostProc :: Maybe PostProcInfo
  , categoryRanking :: [(Maybe Rank, Solver, Score)]
  , categoryJobs :: [JobInfo]
  , categoryComplete :: Bool
  , categoryStartTime :: Maybe UTCTime
  , categoryFinishTime :: Maybe UTCTime
  } deriving (Show)

{-
  TODOs:
    for all categories: show jobIds and their status
      -> mark jobs Complete/Incomplete
    same results -> special case
-}

sortScore :: (Solver, Score) -> (Solver, Score) -> Ordering
sortScore (_,i1) (_,i2) = compare i1 i2

isYes :: SolverResult -> Bool
isYes (YES _) = True
isYes _       = False

-- | output is decreasing by Score
getScores :: [Solver] -> Scoring -> [JobResultInfo] -> [(Solver,Score)]
getScores solver scoring results =
  reverse $ L.sortBy sortScore scores
  where
    scoreMap =
      case scoring of
        Standard -> calcStandardScores results
        Complexity -> calcComplexityScores results
    scores = flip map solver $ \s@(sId,_) ->
              case IM.lookup sId scoreMap of
                Nothing -> (s,0)
                Just scr -> (s,scr)

testGetRanking1 = 
        getRanking [("foo", 30), ("bar", 20), ("what", 20), ("noh", 10)] 
    ==  [(Just 1,"foo",30),(Just 2,"bar",20),(Nothing,"what",20), (Just 4, "noh", 10)]

-- | input is decreasing by score, output has same order of solvers
getRanking  :: (Eq solver, Eq score) 
            => [(solver, score)] -> [(Maybe Int, solver, score)]
getRanking scores =
  let indexedScores = zip [1..] scores 
      equals score (_,(_,scr)) = score == scr
      -- getRanking' :: (Solver, Score) -> (Maybe Rank, Solver, Score)
      getRanking' (solver, score) =
          case filter (equals score) indexedScores of
            [] -> (Nothing, solver, score)
            ((rank,(slv,_)):_) ->
              (if solver /= slv then Nothing else Just rank, solver, score)
  in map getRanking' scores

testCalcScores1 = 
       calcScores [(Just 1,"foo",30),(Just 2,"bar",20),(Nothing,"what",20),(Just 4,"noh",10)]
    == [("foo",3),("bar",2),("what",2),("noh",0)]

-- | input is decreasing by score (?), as computed by getRanking
-- output is, for each solver, the number of solvers that it could beat.

-- FIXME: as the test case shows, evaluation of x is number of y with score y <= score x
-- (not  "<"  as the spec says)

calcScores :: [(Maybe Int, solver, score)] -> [(solver, Int)]
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
    M.toList $ M.fromListWith (+) $ concat $ map calcScores catScores

getCategoriesResult :: Category -> Handler CategoryResult
getCategoriesResult cat = do
  let catName = getCategoryName cat
      catScoring = getCategoryScoring cat
      catPostProcId = getPostProcId cat
      catJobIds = getJobIds cat
  qResults <- queryManyJobs catJobIds
  qPostProc <- queryPostProc catPostProcId
  let results = concat $ map (snd . queryResult) qResults
      solver = getInfo extractSolver results
      scores = getScores solver catScoring results
      rankedSolver = getRanking scores
      jobInfos = catMaybes $ map (fst . queryResult) qResults
      complete = length jobInfos == length catJobIds && all ((==Complete) . jobInfoStatus) jobInfos
      startTime = if null jobInfos
                    then Nothing
                    else Just $ minimum $ map jobInfoStartDate jobInfos
      endTime = if complete
                  then maximum $ map jobInfoFinishDate jobInfos
                  else Nothing
  return $ CategoryResult catName
                          catScoring
                          (queryResult qPostProc)
                          rankedSolver
                          jobInfos
                          complete
                          startTime
                          endTime

getMetaResults :: MetaCategory -> Handler MetaCategoryResult
getMetaResults metaCat = do
  let metaName = getMetaCategoryName metaCat
      categories = getCategories metaCat
  catResults <- mapM getCategoriesResult categories
  let scores = getMetaScores $ map categoryRanking catResults
      ranking = getRanking $ reverse $ L.sortBy sortScore scores
      complete = all categoryComplete catResults
      startTime = if null catResults
                    then Nothing
                    else minimum $ map categoryStartTime catResults
      endTime = if complete
                  then maximum $ map categoryFinishTime catResults
                  else Nothing
  return $ MetaCategoryResult metaName
                              catResults
                              ranking
                              complete
                              startTime
                              endTime

getCompetitionResults :: Competition -> Handler CompetitionResults
getCompetitionResults comp = do
  let compName = getCompetitionName comp
      metaCats = getMetaCategories comp
  metaResults <- mapM getMetaResults metaCats
  let complete = all metaCategoryComplete metaResults
      startTime = if null metaResults
                    then Nothing
                    else minimum $ map metaCategoryStarTime metaResults
      endTime = if complete
                  then maximum $ map metaCategoryFinishTime metaResults
                  else Nothing
  return $ CompetitionResults compName
                              metaResults
                              complete
                              startTime
                              endTime
