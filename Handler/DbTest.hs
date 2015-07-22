module Handler.DbTest where

import Import
import Data.Maybe
import Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- import           Data.Set (Set)
-- import qualified Data.Set as Set
import qualified Data.Set as Set
import Presenter.PersistHelper
import Presenter.Model.Entities()

type JobPairId = Int

data Attribute = 
  ASolverName Text
   | ASlowCpuTime Bool
   | ASolverResult SolverResult
   | ASlowCpuTimeSolverResult Bool SolverResult
 deriving (Eq, Ord, Show)


data JobPairAttributes = JobPairAttributes
  { benchmarkId :: Int
  , solverName  :: Text
  , slowCpuTime :: Bool
  , solverResult :: SolverResult
  } deriving (Show)


-- all job pairs with a response time greater 10 seconds is slow
slowCpuTimeLimit :: ((Num Double, Ord Double)) => Double
slowCpuTimeLimit = 10.0


getDbTestR :: JobID -> Handler Html
getDbTestR jid = do
  jobResults <- getPersistJobResults jid
  let objAttrRel = createObjectAttributeRelation $ getStarExecResults jobResults
  let attrObjRel = createAttributeObjectReleation objAttrRel
  let concepts = createConcepts objAttrRel attrObjRel

  -- let x = Map.toList objAttrRel
  -- let xn = length $ x
  -- #{show xn}
  -- #{show $ length concepts}
    -- <h1>Objects an its attributes
    -- <ul>
    -- $forall (jobPairId, jobPairAttributes) <- Map.toList objAttrRel
    --   <li> #{show jobPairId}: #{show jobPairAttributes}
    -- 
    -- <h1>Attributes with its objects
    -- <ul>
    -- $forall (attr, objects) <- Map.toList attrObjRel
    --   <li> #{show attr}: #{show objects}
  defaultLayout [whamlet|
    <h1>Concepts
    <ul>
    $forall obj <- concepts
      <li> #{show obj}
    |]
    -- <ul>
    -- $forall jobResult <- getStarExecResults jobResults
    --   <li> #{show jobResult}


createConcepts :: Map JobPairId JobPairAttributes -> Map Attribute [JobPairId] -> [([JobPairId], [Attribute])]
-- createConcepts :: Map JobPairId JobPairAttributes -> Map Attribute [JobPairId] -> [([Attribute], [[JobPairId]])]
createConcepts objAttrRel attrObjRel = do
  -- let objects = (take 2000 $ subsequences $ Map.keys objAttrRel :: [[JobPairId]])
  let objects = (subsequences $ Map.keys objAttrRel :: [[JobPairId]])
  -- getCommonAttributes objects objAttrRel
  let attributes = getCommonAttributes objects objAttrRel
  let calculatedObjects = getCommonObjects attributes attrObjRel
  getEqualObjects objects calculatedObjects attributes


getEqualObjects :: [[JobPairId]] -> [[JobPairId]] -> [[Attribute]] -> [([JobPairId],[Attribute])]
getEqualObjects [] _ _ = []
getEqualObjects _ [] _ = []
getEqualObjects _ _ [] = []
getEqualObjects (o:objs) (d:derivedObjs) (a:attrs)
  | o == d = (o,a) : getEqualObjects objs derivedObjs attrs
  | otherwise = getEqualObjects objs derivedObjs attrs


getCommonAttributes :: [[JobPairId]] -> Map JobPairId JobPairAttributes -> [[Attribute]]
--getCommonAttributes :: [[JobPairId]] -> Map JobPairId JobPairAttributes -> [([JobPairId],[Attribute])]
getCommonAttributes jobPairIds objAttrRel = do
    map (\jobs ->
      -- (jobs, getCommonAttribute $ map (\job -> fromJust $ Map.lookup job objAttrRel) jobs))
      getCommonAttribute $ map (\job -> fromJust $ Map.lookup job objAttrRel) jobs)
      jobPairIds


getCommonObjects :: [[Attribute]] -> Map Attribute [JobPairId] -> [[JobPairId]]
getCommonObjects attributes attrObjRel = do
    map (\attrs ->
      -- (attrs, map (\attr -> fromJust $ Map.lookup attr attrObjRel) attrs))
      concat $ map (\attr -> fromJust $ Map.lookup attr attrObjRel) attrs)
      attributes

getCommonAttribute :: [JobPairAttributes] -> [Attribute]
getCommonAttribute attributes = do
    let commonAttr = (getSlowCpuTimeAttribute attributes) ++ (getSolverResultAttribute attributes)
    if elem (ASlowCpuTime True) commonAttr && elem (ASolverResult MAYBE) commonAttr
      then [ASlowCpuTimeSolverResult True MAYBE]
      else if elem (ASlowCpuTime False) commonAttr && elem (ASolverResult MAYBE) commonAttr
        then [ASlowCpuTimeSolverResult False MAYBE]
        else commonAttr




getSlowCpuTimeAttribute :: [JobPairAttributes] -> [Attribute]
getSlowCpuTimeAttribute attributes = do
  if all (\a -> slowCpuTime a == True) attributes
    then [ASlowCpuTime True]
    else if all (\a -> slowCpuTime a == False) attributes
      then [ASlowCpuTime False]
      else []

getSolverResultAttribute :: [JobPairAttributes] -> [Attribute]
getSolverResultAttribute attributes = do
  if all (\a -> solverResult a == MAYBE) attributes
    then [ASolverResult MAYBE]
    else []


createObjectAttributeRelation :: [JobResultInfo] -> Map JobPairId JobPairAttributes
createObjectAttributeRelation jobResults = do
  let jobResultInfoPairIds = map (jobResultInfoPairId) jobResults
  let attrs = getAttributeCollection jobResults
  let objAttrRel = zip jobResultInfoPairIds attrs
  Map.fromList objAttrRel


createAttributeObjectReleation :: Map JobPairId JobPairAttributes -> Map Attribute [JobPairId]
createAttributeObjectReleation objAttrRel = do
  let objAttrs = Map.toList objAttrRel
  -- very ugly and to explicit!
  let cpuTimes = map (\(a,b) -> (a, slowCpuTime b)) objAttrs
  let slowCpuTimes = map (\(a,_) -> a) $ filter (\(_,b) -> b == True) cpuTimes
  let fastCpuTimes = map (\(a,_) -> a) $ filter (\(_,b) -> b == False) cpuTimes
  
  let solverResults = map (\(a,b) -> (a, solverResult b)) objAttrs
  let maybeSolverResults = map (\(a,_) -> a) $ filter (\(_,b) -> b == MAYBE) solverResults
  -- let otherSolverResults = map (\(a,_) -> a) $ filter (\(_,b) -> b == OTHER) solverResults
  let attrsObjRel = Map.insert (ASlowCpuTime True) slowCpuTimes Map.empty
  let attrsObjRel' = Map.insert (ASlowCpuTime False) fastCpuTimes attrsObjRel

  -- only helper keys so the union does not have to be calculated again and again
  let attrsObjRel'' = Map.insert (ASlowCpuTimeSolverResult False MAYBE) (listUnion slowCpuTimes maybeSolverResults) attrsObjRel'
  let attrsObjRel''' = Map.insert (ASlowCpuTimeSolverResult True MAYBE) (listUnion fastCpuTimes maybeSolverResults) attrsObjRel''
  Map.insert (ASolverResult MAYBE) maybeSolverResults attrsObjRel'''


getAttributeCollection :: [JobResultInfo] -> [JobPairAttributes]
getAttributeCollection jobResults = do
  -- would it be better to remove some attributes of the existing record?
  let jobResultInfoBenchmarkIds = map (jobResultInfoBenchmarkId) jobResults
  let jobResultInfoSolvers = map (jobResultInfoSolver) jobResults
  let cpuTimeEvaluations = evaluateCpuTime jobResults
  let jobResultInfoResults = map (jobResultInfoResult) jobResults
  let attrs = zip4 jobResultInfoBenchmarkIds jobResultInfoSolvers cpuTimeEvaluations jobResultInfoResults
  map (\(a,b,c,d)-> JobPairAttributes {benchmarkId = a, solverName = b, slowCpuTime = c, solverResult = d}) attrs

evaluateCpuTime :: [JobResultInfo] -> [Bool]
evaluateCpuTime = map ((> slowCpuTimeLimit). jobResultInfoCpuTime)


-- https://github.com/nh2/haskell-ordnub
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs
-- https://github.com/nh2/haskell-ordnub
listUnion :: (Ord a) => [a] -> [a] -> [a]
listUnion a b = a ++ ordNub (filter (`Set.notMember` aSet) b)
  where
    aSet = Set.fromList a

updateMap :: (Ord k, Num a) => k -> a -> Map k a -> Map k a
updateMap = Map.insertWith (+)
