module Presenter.Processing where

import Prelude
import Presenter.Model
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Debug.Trace
import qualified Data.IntMap.Strict as IM

type BenchmarkName = Name
type Solver = (SolverID, SolverName)
type SolverName = Name
type SolverResults = [Maybe SolverResult]
type BenchmarkRow = (Benchmark, [Maybe JobResultInfo])
type TableHead = [SolverName]

getClass :: JobResultInfo -> T.Text
getClass result =
  case jobResultInfoResult result of
    YES _     -> "solver-yes"
    NO        -> "solver-no"
    MAYBE     -> "solver-maybe"
    CERTIFIED -> "solver-certified"
    ERROR     -> "solver-error"
    _         -> "solver-nothing"

getInfo :: (JobResultInfo -> S.Set a -> S.Set a) -> [JobResultInfo] -> [a]
getInfo f = S.toList . L.foldr f S.empty

getBenchmark :: JobResultInfo -> (BenchmarkID, BenchmarkName)
getBenchmark jri = (jobResultInfoBenchmarkId jri, jobResultInfoBenchmark jri)

extractBenchmark :: JobResultInfo -> S.Set (BenchmarkID, BenchmarkName) -> S.Set (BenchmarkID, BenchmarkName)
extractBenchmark jri set =
  S.insert (getBenchmark jri) set

getSolver :: JobResultInfo -> (SolverID, SolverName)
getSolver jri = (jobResultInfoSolverId jri, jobResultInfoSolver jri)

extractSolver :: JobResultInfo -> S.Set (SolverID, SolverName) -> S.Set (SolverID, SolverName)
extractSolver jri set =
  S.insert (getSolver jri) set

getBenchmarkResults :: [(Int, (SolverID, SolverName))] -> [JobResultInfo] -> [(BenchmarkID, BenchmarkName)] -> [BenchmarkRow]
getBenchmarkResults solvers jobInfos = map getBenchmarkRow 
  where
    getBenchmarkRow benchmark@(bId, _) =
      (benchmark, map (getSolverResults bId) solvers)
    getSolverResults _benchmarkId (i, (sId, _)) =
      let mResult = L.find (isResult _benchmarkId sId i) jobInfos
      in case mResult of
        Just result -> Just result
        Nothing -> Nothing
    isResult _benchmarkId _solverId _jobId jri =
      (jobResultInfoBenchmarkId jri == _benchmarkId) &&
        (jobResultInfoSolverId jri == _solverId) &&
        (jobResultInfoJobId jri == _jobId)

compareBenchmarks :: (BenchmarkID, BenchmarkName) -> (BenchmarkID, BenchmarkName) -> Ordering
compareBenchmarks (_,n0) (_,n1) = compare n0 n1

getScore :: JobResultInfo -> Int
getScore jri = case jobResultInfoScore jri of
                  Just i -> i
                  _      -> 0

getScoredResults :: [JobResultInfo] -> [JobResultInfo]
getScoredResults results =
  let insertResult m result = M.insertWith (++) (getBenchmark result) [result] m
      benchmarkMap = M.toList $ L.foldl' (insertResult) M.empty results
      result = L.foldl' (++) [] $ map (calcScores . snd) benchmarkMap
  in result
  where
    calcScores :: [JobResultInfo] -> [JobResultInfo]
    calcScores jris =
      if any (\r -> jobResultInfoStatus r /= JobResultComplete) jris
        then jris
        else
          let hasScore jri = case jobResultInfoResult jri of
                                YES (Just _) -> True
                                _            -> False
              getScoreFromResult jri =
                case jobResultInfoResult jri of
                  YES (Just i) -> i
                  _            -> 0
              insertScore jri set = S.insert (getScoreFromResult jri) set
              scores = S.toList $ L.foldr insertScore S.empty $ filter hasScore jris
              baseScore = 1 + length jris
              scoreIndex jri =
                case L.elemIndex (getScoreFromResult jri) scores of
                  Nothing -> negate baseScore
                  Just i  -> i
              updateScore jri = jri {
                                  jobResultInfoScore = Just $
                                    if hasScore jri
                                      then baseScore - scoreIndex jri
                                      else 0
                                }
          in map updateScore jris

calcScoresBase :: (JobResultInfo -> Int) -> [JobResultInfo] -> IM.IntMap Int
calcScoresBase getScore = L.foldl' addScore IM.empty
  where addScore m jri = IM.insertWith (+) (jobResultInfoSolverId jri) (getScore jri) m

calcComplexityScores :: [JobResultInfo] -> IM.IntMap Int
calcComplexityScores = calcScoresBase getScore
  where
    getScore jri =
      case jobResultInfoScore jri of
        Nothing -> 0
        Just i  -> i

calcStandardScores :: [JobResultInfo] -> IM.IntMap Int
calcStandardScores = calcScoresBase getScore
  where
    getScore jri =
      case jobResultInfoResult jri of
        YES _ -> 1
        NO    -> 1
        _     -> 0
