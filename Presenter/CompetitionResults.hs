-- | semantics specification:
-- In each meta-category, a medal will be awarded to the highest-scoring solver.
-- For every meta-category, we consider the sum of the scores 
-- for each category within that meta-category: 
-- The score of a tool is determined by the number of other tools
-- which could be beaten in that category. 
-- http://www.termination-portal.org/wiki/Termination_Competition_2014#Competition_Categories_and_Awards

module Presenter.CompetitionResults 
  ( getCompetitionResults
  , CompetitionResults(..)
  , MetaCategoryResult(..)
  , CategoryResult(..)
  ) where

import Import
import Presenter.StarExec.JobData
import Presenter.Processing
import Presenter.Statistics
import Presenter.StarExec.Connection
import Presenter.StarExec.Commands
import Presenter.PersistHelper
import qualified Data.List as L
import Data.Maybe
import Data.Time.Clock
import qualified Data.IntMap.Strict as IM

import qualified Data.Map.Strict as M

type PostProcInfoMap = IM.IntMap PostProcInfo
type JobInfoMap = M.Map JobID Job
type JobResultsMap = M.Map JobID [JobResult]

sortScore :: (a, Score) -> (a, Score) -> Ordering
sortScore (_,i1) (_,i2) = compare i1 i2

isYes :: SolverResult -> Bool
isYes (YES _) = True
isYes _       = False

-- | output is decreasing by Score
getScores :: [UniqueSolver] -> Scoring -> [JobResult] -> [(UniqueSolver,Score)]
getScores solver scoring results =
  reverse $ L.sortBy sortScore scores
  where
    scoreMap =
      case scoring of
        Standard -> calcStandardScores results
        Complexity -> calcComplexityScores results
    scores = flip map solver $ \s@(sId,_) ->
              case M.lookup sId scoreMap of
                Nothing -> (s,0)
                Just scr -> (s,scr)

--testGetRanking1 = 
--        getRanking [("foo", 30), ("bar", 20), ("what", 20), ("noh", 10)] 
--    ==  [toRank (Just 1,"foo",30), toRank (Just 2,"bar",20), toRank (Nothing,"what",20), toRank (Just 4, "noh", 10)]
--    where
--      toRank (r,slv,scr) = SolverRankEntry r slv scr

-- | input is decreasing by score, output has same order of solvers
getRanking  :: [(UniqueSolver, Score)] -> [SolverRankEntry]
getRanking scores =
  let indexedScores = zip [1..] scores 
      equals score (_,(_,scr)) = score == scr
      getRanking' :: (UniqueSolver, Score) -> SolverRankEntry
      getRanking' (solver, score) =
          case filter (equals score) indexedScores of
            [] -> SolverRankEntry Nothing solver score
            ((rank,(slv,_)):_) -> SolverRankEntry
              { rank = if solver /= slv then Nothing else Just rank
              , solver = solver
              , score = score
              }
  in map getRanking' scores

--testCalcScores1 = 
--       calcScores [(Just 1,"foo",30),(Just 2,"bar",20),(Nothing,"what",20),(Just 4,"noh",10)]
--    == [("foo",3),("bar",2),("what",2),("noh",0)]

-- | input is decreasing by score (?), as computed by getRanking
-- output is, for each solver, the number of solvers that it could beat.

-- FIXME: as the test case shows, evaluation of x is number of y with score y <= score x
-- (not  "<"  as the spec says)

calcScores :: [SolverRankEntry] -> [(UniqueSolver, Int)]
calcScores [entry] = [(solver entry, 0)]
calcScores ranking = calcScores' 1 ranking
  where
    numRankings = length ranking
    calcScores' rnk ((entry):rs) =
      let mRank = rank entry
          slv = solver entry
      in case mRank of
          Nothing -> (slv,numRankings - rnk):calcScores' rnk rs
          Just r -> (slv,numRankings - r):calcScores' r rs
    calcScores' _ [] = []

getMetaScores :: [[SolverRankEntry]] -> [(UniqueSolver, Score)]
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
      complete = length jobInfos == length catJobIds && all ((==Complete) . toJobStatus) jobInfos
      startTime = if null jobInfos
                    then Nothing
                    else minimum <$> (sequence $ map toJobStartDate jobInfos)
      endTime = if complete && not (null jobInfos)
                  then maximum <$> (sequence $ map toJobFinishDate jobInfos)
                  else Nothing

      stat = ( mconcat $ map jobStat results )
           { startTime = startTime, finishTime = endTime }

  return $ CategoryResult catName
                          catScoring
                          (queryResult qPostProc)
                          rankedSolver
                          jobInfos
                          complete
                          startTime
                          endTime
                          stat

lookupMap :: Ord k => M.Map k a -> k -> Maybe a
lookupMap = flip M.lookup

getCategoriesResult_ :: PostProcInfoMap -> JobInfoMap -> JobResultsMap -> Category -> CategoryResult
getCategoriesResult_ procs infos results cat =
  let catName = getCategoryName cat
      catScoring = getCategoryScoring cat
      catPostProcId = getPostProcId cat
      catJobIds = getJobIds cat
      catResults = catMaybes $ map (lookupMap results) catJobIds
      catInfos = catMaybes $ map (lookupMap infos) catJobIds
      catPostProc = IM.lookup catPostProcId procs
      jobResults = concat $ catResults
      solver = getInfo extractSolver jobResults
      scores = getScores solver catScoring jobResults
      rankedSolver = getRanking scores
      jobInfos = catInfos
      complete = length jobInfos == length catJobIds && all ((==Complete) . toJobStatus) jobInfos
      startTime = if null jobInfos
                    then Nothing
                    else minimum <$> (sequence $ map toJobStartDate jobInfos)
--                    else Just $ minimum $ map jobInfoStartDate jobInfos
      endTime = if complete && not (null jobInfos)
                  then maximum <$> (sequence $ map toJobFinishDate jobInfos)
--                  then maximum $ map jobInfoFinishDate jobInfos
                  else Nothing
      cpuTotal = sum $ map toCpuTime jobResults
      wallTotal = sum $ map toWallclockTime jobResults
      stat = Statistics { complete = complete 
                        , pairs = length jobResults
                        , pairsCompleted = length 
                              $ filter ( ( == JobResultComplete) . getResultStatus ) jobResults
                        , startTime = startTime, finishTime = endTime 
                        , cpu = cpuTotal, wallclock = wallTotal
                        }
  in CategoryResult catName
                    catScoring
                    catPostProc
                    rankedSolver
                    jobInfos
                    complete
                    startTime
                    endTime
                    stat

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
      stat = mconcat $ map categoryStatistics catResults
      
  return $ MetaCategoryResult metaName
                              catResults
                              ranking
                              complete
                              startTime
                              endTime
                              stat

getMetaResults_ :: PostProcInfoMap -> JobInfoMap -> JobResultsMap -> MetaCategory -> MetaCategoryResult
getMetaResults_ procs infos results metaCat =
  let metaName = getMetaCategoryName metaCat
      categories = getCategories metaCat
      catResults = map (getCategoriesResult_ procs infos results) categories
      scores = getMetaScores $ map categoryRanking catResults
      ranking = getRanking $ reverse $ L.sortBy sortScore scores
      complete = all categoryComplete catResults
      startTime = if null catResults
                    then Nothing
                    else minimum $ map categoryStartTime catResults
      endTime = if complete
                  then maximum $ map categoryFinishTime catResults
                  else Nothing
      stat = mconcat $ map categoryStatistics catResults
  in MetaCategoryResult metaName
                        catResults
                        ranking
                        complete
                        startTime
                        endTime
                        stat

filterMaybeTuple :: (a, Maybe b) -> Bool
filterMaybeTuple (_,Nothing) = False
filterMaybeTuple _ = True

fromMaybeTuple :: (a, Maybe b) -> (a,b)
fromMaybeTuple (a, Just b) = (a,b)

updateJobInfo' :: JobInfo -> YesodDB App ()
updateJobInfo' ji = do
  mPersistInfo <- getBy $ UniqueJobInfo $ jobInfoStarExecId ji
  case mPersistInfo of
    Nothing -> updateJobInfo Nothing ji
    Just en -> updateJobInfo (Just $ entityVal en) ji

getProcessedResults :: (Maybe Job,[JobResult]) -> [JobResult]
getProcessedResults (mJobInfo, results) =
  case mJobInfo of
    Just ji -> if isComplexity ji
                then getScoredResults results
                else results
    Nothing -> results

updateJob :: UTCTime -> (Maybe JobInfo, Maybe JobInfo) -> Maybe JobInfo
updateJob _ (Nothing, Nothing) = Nothing
updateJob _ ((Just ji), Nothing) = Just ji
updateJob currentTime (Nothing, (Just ji)) = Just ji
  { jobInfoFinishDate = if jobInfoStatus ji == Complete
                          then Just currentTime
                          else Nothing
  , jobInfoLastUpdate = currentTime
  }
updateJob currentTime ((Just persist), (Just starexec)) = Just persist
    { jobInfoName = jobInfoName starexec
    , jobInfoStatus = jobInfoStatus starexec
    , jobInfoDate = jobInfoDate starexec
    , jobInfoPreProc = jobInfoPreProc starexec
    , jobInfoPostProc = jobInfoPostProc starexec
    , jobInfoIsComplexity = jobInfoIsComplexity starexec
    , jobInfoFinishDate = case jobInfoFinishDate persist of
                            Nothing -> if jobInfoStatus starexec == Complete
                                        then Just currentTime
                                        else Nothing
                            Just fd -> Just fd
    , jobInfoLastUpdate = currentTime
    }

updateJobs :: UTCTime -> [(Maybe Job, Maybe Job)] -> [Maybe Job]
updateJobs now = map updateJob'
  where
    updateJob' :: (Maybe Job, Maybe Job) -> Maybe Job
    updateJob' (Nothing, Nothing) = Nothing
    updateJob' (j@(Just _), Nothing) = j
    updateJob' (Nothing, (Just (StarExecJob j))) = StarExecJob <$> updateJob now (Nothing, Just j)
    updateJob' (Nothing, j@(Just _)) = j
    updateJob' (Just (StarExecJob j1), Just (StarExecJob j2)) = StarExecJob <$> updateJob now (Just j1, Just j2)

maybeJobComplete :: Maybe Job -> Bool
maybeJobComplete Nothing = False
maybeJobComplete (Just ji) = toJobStatus ji == Complete

getJobInfos :: StarExecConnection -> [JobID] -> Handler [Maybe Job]
getJobInfos con = mapM fetchJobInfo
  where
    fetchJobInfo :: JobID -> Handler (Maybe Job)
    fetchJobInfo (StarExecJobID j) = do
      ji <- getJobInfo con j
      return (StarExecJob <$> ji)
    fetchJobInfo j = getPersistJobInfo j

getJobResults' :: StarExecConnection -> [JobID] -> Handler [[JobResult]]
getJobResults' con = mapM fetchResults
  where
    fetchResults :: JobID -> Handler [JobResult]
    fetchResults (StarExecJobID j) = do
      results <- getJobResults con j
      return (StarExecResult <$> results)
    fetchResults j = getPersistJobResults j

getCompetitionResults :: Competition -> Handler CompetitionResults
getCompetitionResults comp = do
  let compMeta = getMetaData comp
      compName = getCompetitionName comp
      metaCats = getMetaCategories comp
      cats = concat $ map getCategories metaCats
      jobIds = concat $ map getJobIds cats
      postProcIds = L.nub $ map getPostProcId cats
  persistJobInfos <- mapM getPersistJobInfo jobIds
  persistPostProcs <- mapM getPersistPostProcInfo postProcIds
  let jobsComplete = all maybeJobComplete persistJobInfos
      hasAllPostProcs = all isJust persistPostProcs
  -- liftIO $ putStrLn $ "jobs complete: " ++ (show jobsComplete)
  con <- getConnection
  postProcs <- if hasAllPostProcs
                then return persistPostProcs
                else mapM (getPostProcInfo con) postProcIds
  jobInfos <- if jobsComplete
                then return persistJobInfos
                else getJobInfos con jobIds
  jobResults <- if jobsComplete
                  then mapM (getPersistJobResults) jobIds
                  else getJobResults' con jobIds
  currentTime <- lift getCurrentTime
  let updatedJobs = if jobsComplete
                    then jobInfos
                    else updateJobs currentTime $ zip persistJobInfos jobInfos
      jobs = zip jobInfos jobResults
      processedResults = map getProcessedResults jobs

  runDB_exclusive $ do
    if not hasAllPostProcs
      then mapM_ updatePostProcInfo $ catMaybes postProcs
      else return ()
    if not jobsComplete
      then do
        mapM_ updateJobInfo' $ getStarExecJobs $ catMaybes updatedJobs
        updateJobResults $ getStarExecResults $ concat processedResults
      else return ()

  let postProcMap = IM.fromList $ map fromMaybeTuple $ filter filterMaybeTuple $ zip postProcIds postProcs
      jobInfoMap = M.fromList $ map fromMaybeTuple $ filter filterMaybeTuple $ zip jobIds updatedJobs
      jobResultsMap = M.fromList $ zip jobIds processedResults
      metaResults = map (getMetaResults_ postProcMap jobInfoMap jobResultsMap) metaCats
      complete = all metaCategoryComplete metaResults
      startTime = if null metaResults
                    then Nothing
                    else minimum $ map metaCategoryStarTime metaResults
      endTime = if complete
                  then maximum $ map metaCategoryFinishTime metaResults
                  else Nothing
      stat = mconcat $ map metaCategoryStatistics metaResults
  return $ CompetitionResults compMeta
                              metaResults
                              complete
                              startTime
                              endTime
                              stat
