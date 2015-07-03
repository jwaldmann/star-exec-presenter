module Presenter.Processing where

import Prelude
import Presenter.Model
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type BenchmarkName = Name
type UniqueBenchmark = (BenchmarkID, BenchmarkName)
--type UniqueSolver = (SolverID, SolverName)
--type SolverName = Name
type SolverResults = [Maybe SolverResult]
type BenchmarkRow = (UniqueBenchmark, [Maybe JobResult])
type TableHead = [SolverName]

getClass :: JobResult -> T.Text
getClass result =
  case getSolverResult result of
    YES _     -> "solver-yes"
    NO        -> "solver-no"
    MAYBE     -> "solver-maybe"
    CERTIFIED -> "solver-certified"
    ERROR     -> "solver-error"
    _         -> "solver-nothing"

getInfo :: (JobResult -> S.Set a -> S.Set a) -> [JobResult] -> [a]
getInfo f = S.toList . L.foldr f S.empty

getBenchmark :: JobResult -> UniqueBenchmark
getBenchmark jr = (toBenchmarkID jr, toBenchmarkName jr)

extractBenchmark :: JobResult -> S.Set UniqueBenchmark -> S.Set UniqueBenchmark
extractBenchmark jr set =
  S.insert (getBenchmark jr) set

getSolver :: JobResult -> (SolverID, SolverName)
getSolver jr = (toSolverID jr, toSolverName jr)

extractSolver :: JobResult -> S.Set (SolverID, SolverName) -> S.Set (SolverID, SolverName)
extractSolver jr set =
  S.insert (getSolver jr) set

getBenchmarkResults :: [(JobID, UniqueSolver)] -> [JobResult] -> [UniqueBenchmark] -> [BenchmarkRow]
getBenchmarkResults solvers jobInfos = map getBenchmarkRow 
  where
    --getBenchmark :: UniqueBenchmark -> BenchmarkRow
    getBenchmarkRow benchmark@(bId, _) =
      (benchmark, map (getSolverResults bId) solvers)
    --getSolverResults :: BenchmarkID
    getSolverResults _benchmarkId (i, (sId, _)) =
      let mResult = L.find (isResult _benchmarkId sId i) jobInfos
      in case mResult of
        Just result -> Just result
        Nothing -> Nothing
    isResult _benchmarkId _solverId _jobId jr =
      (toBenchmarkID jr == _benchmarkId) &&
        (toSolverID jr == _solverId) &&
        (getJobID jr == _jobId)

compareBenchmarks :: UniqueBenchmark -> UniqueBenchmark -> Ordering
compareBenchmarks (_,n0) (_,n1) = compare n0 n1

getScore :: JobResult -> Int
getScore jr = case toScore jr of
                  Just i -> i
                  _      -> 0

getScoredResults :: [JobResult] -> [JobResult]
getScoredResults results =
  let insertResult m r = M.insertWith (++) (getBenchmark r) [r] m
      benchmarkMap = M.toList $ L.foldl' (insertResult) M.empty results
      result = L.foldl' (++) [] $ map (calcScores . snd) benchmarkMap
  in result
  where
    calcScores :: [JobResult] -> [JobResult]
    calcScores jrs =
      if any (\r -> getResultStatus r /= JobResultComplete) jrs
        then jrs
        else
          let hasScore jr = case getSolverResult jr of
                                YES (Just _) -> True
                                _            -> False
              getScoreFromResult jr =
                case getSolverResult jr of
                  YES (Just i) -> i
                  _            -> 0
              insertScore jr set = S.insert (getScoreFromResult jr) set
              scores = S.toList $ L.foldr insertScore S.empty $ filter hasScore jrs
              baseScore = 1 + length jrs
              scoreIndex jr =
                case L.elemIndex (getScoreFromResult jr) scores of
                  Nothing -> negate baseScore
                  Just i  -> i
              setScore jri = updateScore jri $ Just $
                                if hasScore jri
                                  then baseScore - scoreIndex jri
                                  else 0
          in map setScore jrs

calcScoresBase :: (JobResult -> Int) -> [JobResult] -> M.Map SolverID Int
calcScoresBase f = L.foldl' addScore M.empty
  where addScore m jr = M.insertWith (+) (toSolverID jr) (f jr) m

calcComplexityScores :: [JobResult] -> M.Map SolverID Int
calcComplexityScores = calcScoresBase getScore'
  where
    getScore' jr =
      case toScore jr of
        Nothing -> 0
        Just i  -> i

calcStandardScores :: [JobResult] -> M.Map SolverID Int
calcStandardScores = calcScoresBase getScore'
  where
    getScore' jr =
      case getSolverResult jr of
        YES _ -> 1
        NO    -> 1
        _     -> 0
