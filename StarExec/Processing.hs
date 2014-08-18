module StarExec.Processing where

import Prelude
import Model
import StarExec.Types
import Presenter.RouteTypes
import Presenter.Models
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

type BenchmarkName = T.Text
type BenchmarkIdent = (BenchmarkID, BenchmarkName)
type SolverIdent = (SolverID, SolverName)
type SolverName = T.Text
type SolverResults = [Maybe SolverResult]
type BenchmarkRow = (BenchmarkIdent, [Maybe JobResult])
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

getBenchmark :: JobResult -> BenchmarkIdent
getBenchmark jri = (toBenchmarkID jri, toBenchmarkName jri)

extractBenchmark :: JobResult -> S.Set BenchmarkIdent -> S.Set BenchmarkIdent
extractBenchmark jri set =
  S.insert (getBenchmark jri) set

getSolver :: JobResult -> SolverIdent
getSolver jri = (toSolverID jri, toSolverName jri)

extractSolver :: JobResult -> S.Set SolverIdent -> S.Set SolverIdent
extractSolver jri set =
  S.insert (getSolver jri) set

getBenchmarkResults :: [(JobID, SolverIdent)] -> [JobResult] -> [BenchmarkIdent] -> [BenchmarkRow]
getBenchmarkResults solvers jobInfos = map getBenchmarkRow 
  where
    getBenchmarkRow :: BenchmarkIdent -> BenchmarkRow
    getBenchmarkRow benchmark@(bId, _) =
      (benchmark, map (getSolverResults bId) solvers)
    getSolverResults :: BenchmarkID -> (JobID, SolverIdent) -> Maybe JobResult
    getSolverResults _benchmarkId (i, (sId, _)) =
      let mResult = L.find (isResult _benchmarkId sId i) jobInfos
      in case mResult of
        Just result -> Just result
        Nothing -> Nothing
    isResult :: BenchmarkID -> SolverID -> JobID -> JobResult -> Bool
    isResult _benchmarkId _solverId _jobId resultInfo =
      (toBenchmarkID resultInfo == _benchmarkId) &&
      (toSolverID resultInfo == _solverId) &&
      (toJobID resultInfo == _jobId)

compareBenchmarks :: BenchmarkIdent -> BenchmarkIdent -> Ordering
compareBenchmarks (_,n0) (_,n1) = compare n0 n1

getScore :: JobResultInfo -> Int
getScore jri = case jobResultInfoScore jri of
                  Just i -> i
                  _      -> 0

getScoredResults :: [JobResult] -> [JobResult]
getScoredResults results =
  let insertResult m result = M.insertWith (++) (getBenchmark result) [result] m
      benchmarkMap = M.toList $ L.foldl' (insertResult) M.empty results
      result = L.foldl' (++) [] $ map (calcScores . snd) benchmarkMap
  in result
  where
    calcScores :: [JobResult] -> [JobResult]
    calcScores jris =
      if any isResultComplete jris
        then jris
        else
          let hasScore jri = case getSolverResult jri of
                                YES (Just _) -> True
                                _            -> False
              getScoreFromResult jri =
                case getSolverResult jri of
                  YES (Just i) -> i
                  _            -> 0
              insertScore jri set = S.insert (getScoreFromResult jri) set
              scores = S.toList $ L.foldr insertScore S.empty $ filter hasScore jris
              baseScore = 1 + length jris
              scoreIndex jri =
                case L.elemIndex (getScoreFromResult jri) scores of
                  Nothing -> negate baseScore
                  Just i  -> i
              updateScore' jri = updateScore jri $ Just $
                                    if hasScore jri
                                      then baseScore - scoreIndex jri
                                      else 0
          in map updateScore' jris

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
