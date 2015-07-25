module Presenter.Processing where

import Prelude
import Presenter.Model
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad ( guard )

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
    YES       -> "solver-yes"
    NO        -> "solver-no"
    MAYBE     -> "solver-maybe"
    BOUNDS {} -> "solver-bounds"
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


-- | this is called to compute the total score (for a solver, in a category).
-- it is applied to results that are already scored (see scoredResults function)
calculateScores :: Scoring -> [JobResult] -> M.Map SolverID Int
calculateScores sc jps = M.fromListWith (+) $ do
  jp <- jps ; return ( toSolverID jp, getScore jp )

-- | this is called to compute the scores per jobpair (a cell in the table).
scoredResults :: Scoring -> [ JobResult ] -> [ JobResult ]
scoredResults sc jps = do
  let bybench = M.fromListWith (++) $ do
        jp <- jps ; return ( getBenchmark jp, [ jp ] )
      points = M.fromListWith (+)  $ do
        (bench,jps) <- M.toList bybench
        (solver,points) <- M.toList $ M.unionsWith (+) $ case sc of
              Complexity -> [ upperpoints jps, lowerpoints jps ]
              Standard ->  [ standardpoints jps ]
        return ((bench,solver),points)
  jp <- jps
  return $ updateScore jp $ M.lookup (getBenchmark jp,toSolverID jp) points


standardpoints jps = M.fromList $ do
  me <- jps
  let points = case getSolverResult me of
        YES -> 1 ; NO -> 1 ; _ -> 0
  return ( toSolverID me, points )

uppertrivial = Infinite
getupperbound jp = case getSolverResult jp of
              BOUNDS bounds -> upper bounds
              YES -> Finite
              _ -> uppertrivial
upperpoints jps = M.fromListWith (+) $ do
              me <- jps
              let greaterequals = length $ do
                    you <- jps 
                    guard $ compare_for_upper_bounds
                      (getupperbound me) (getupperbound you) <= EQ
                  points = if getupperbound me == uppertrivial then 0 else greaterequals
              return ( toSolverID me , points )

lowertrivial = Finite
getlowerbound jp = case getSolverResult jp of
              BOUNDS bounds -> lower bounds
              NO -> Infinite
              _ -> lowertrivial
lowerpoints jps = M.fromListWith (+) $ do
              me <- jps
              let greaterequals = length $ do
                    you <- jps 
                    guard $ compare_for_lower_bounds
                      (getlowerbound me) (getlowerbound you) >= EQ
                  points = if getlowerbound me == lowertrivial then 0 else greaterequals
              return ( toSolverID me , points )
              
