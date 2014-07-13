module StarExec.Processing where

import Import
import StarExec.Types
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Debug.Trace

type BenchmarkID = Int
type BenchmarkName = T.Text
type Benchmark = (BenchmarkID, BenchmarkName)
type SolverID = Int
type Solver = (SolverID, SolverName)
type SolverName = T.Text
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

getBenchmark :: JobResultInfo -> Benchmark
getBenchmark jri = (jobResultInfoBenchmarkId jri, jobResultInfoBenchmark jri)

extractBenchmark :: JobResultInfo -> S.Set Benchmark -> S.Set Benchmark
extractBenchmark jri set =
  S.insert (getBenchmark jri) set

getSolver :: JobResultInfo -> Solver
getSolver jri = (jobResultInfoSolverId jri, jobResultInfoSolver jri)

extractSolver :: JobResultInfo -> S.Set Solver -> S.Set Solver
extractSolver jri set =
  S.insert (getSolver jri) set

getBenchmarkResults :: [Solver] -> [JobResultInfo] -> [Benchmark] -> [BenchmarkRow]
getBenchmarkResults solvers jobInfos = map getBenchmarkRow 
  where
    getBenchmarkRow benchmark@(bId, _) =
      (benchmark, map (getSolverResults bId) solvers)
    getSolverResults _benchmarkId (sId, _) =
      let mResult = L.find (isResult _benchmarkId sId) jobInfos
      in case mResult of
        Just result -> Just result
        Nothing -> Nothing
    isResult _benchmarkId _solverId jri =
      (jobResultInfoBenchmarkId jri == _benchmarkId) &&
        (jobResultInfoSolverId jri == _solverId)

compareBenchmarks :: Benchmark -> Benchmark -> Ordering
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
