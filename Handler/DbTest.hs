module Handler.DbTest where

import Import
import Data.List
import Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- import           Data.Set (Set)
-- import qualified Data.Set as Set
import Presenter.PersistHelper
import Presenter.Model.Entities()


-- all job pairs with a response time greater 10 seconds is slow
slowCpuTimeLimit :: ((Num Double, Ord Double)) => Double
slowCpuTimeLimit = 10.0

getDbTestR :: JobID -> Handler Html
getDbTestR jid = do
  jobResults <- getPersistJobResults jid
  let jobResultInfos = createObjectAttributeRelations $ getStarExecResults jobResults
  let concepts = createConcepts jobResultInfos

  let x = Map.toList jobResultInfos
  let xn = length $ x
  defaultLayout [whamlet|
    #{show xn}
    #{show $ length concepts}
    <ul>
    $forall (jobPairId, jobPairAttributes) <- Map.toList jobResultInfos
      <li> #{show jobPairId}: #{show jobPairAttributes}
    |]
    -- <ul>
    -- $forall jobResult <- getStarExecResults jobResults
    --   <li> #{show jobResult}


createConcepts :: Map Int (Int, Text, Bool, SolverResult) -> [[Int]]
createConcepts jobResultInfos = do
  let keys = getPowerset $ Map.keys jobResultInfos
  if isJust keys
    then concat $ maybeToList keys
    else [[42::Int]]
  -- let concepts = 
  -- [(,)]


getPowerset :: [Int] -> Maybe [[Int]]
getPowerset set =
  if isSubseqToBig set
    then Nothing
    else Just $ subsequences set


-- condition to do not calculate powersets of a set with more than 26 elements
isSubseqToBig :: [Int] -> Bool
isSubseqToBig l =
  if length l > 27
    then True
    else False


createObjectAttributeRelations :: [JobResultInfo] -> Map Int (Int, Text, Bool, SolverResult)
createObjectAttributeRelations jobResults = do
  let jobResultInfoPairIds = map (jobResultInfoPairId) jobResults
  let attrs = getAttributeCollection jobResults
  let objAttrRel = zip jobResultInfoPairIds attrs
  Map.fromList objAttrRel


getAttributeCollection :: [JobResultInfo] -> [(Int, Text, Bool, SolverResult)]
getAttributeCollection jobResults = do
  let cpuTimeEvaluations = evaluateCpuTime jobResults
  let jobResultInfoSolvers = map (jobResultInfoSolver) jobResults
  let jobResultInfoResults = map (jobResultInfoResult) jobResults
  let jobResultInfoBenchmarkIds = map (jobResultInfoBenchmarkId) jobResults
  zip4 jobResultInfoBenchmarkIds jobResultInfoSolvers cpuTimeEvaluations jobResultInfoResults


evaluateCpuTime :: [JobResultInfo] -> [Bool]
evaluateCpuTime = map (isLow . jobResultInfoCpuTime)

isLow :: (Num Double, Ord Double) => Double -> Bool
isLow a = a > slowCpuTimeLimit

-- evaluateResult :: [JobResultInfo] -> [SolverResult]
-- evaluateResult = map (jobResultInfoResult)

-- copied from: http://rosettacode.org/wiki/Power_set#Haskell
-- powerset :: Foldable t => t a -> [[a]]
-- powerset = foldr (\x acc -> acc ++ map (x:) acc) [[]]


-- rfilterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- rfilterM f []     = return []
-- rfilterM f (x:xs) = do mxs <- rfilterM f xs; pred <- f x
--                        if pred then return (x:mxs)
--                                else return mxs
-- 
-- powerset' = rfilterM (const [True,False])
