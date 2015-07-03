module Presenter.Model.RouteTypes where

import Prelude
import Yesod
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Presenter.Internal.Stringish
import Presenter.Model.Query
import Presenter.Internal.Stringish

type Prefix = T.Text

lriResultPrefix :: Prefix
lriResultPrefix = "lri_result."

uibkResultPrefix :: Prefix
uibkResultPrefix = "uibk_result."

lriJobPrefix :: Prefix
lriJobPrefix = "lri_job."

uibkJobPrefix :: Prefix
uibkJobPrefix = "uibk_job."

lriPairPrefix :: Prefix
lriPairPrefix = "lri_pair."

uibkPairPrefix :: Prefix
uibkPairPrefix = "uibk_pair."

lriSolverPrefix :: Prefix
lriSolverPrefix = "lri_solver."

uibkSolverPrefix :: Prefix
uibkSolverPrefix = "uibk_solver."

lriBenchmarkPrefix :: Prefix
lriBenchmarkPrefix = "lri_benchmark."

uibkBenchmarkPrefix :: Prefix
uibkBenchmarkPrefix = "uibk_benchmark."

{-
-}
data ErrorID =
  LoginError
  | Unkown
  deriving (Eq, Show, Read)

{-
-}
data JobID =
  StarExecJobID Int
  | LriJobID Int
  | UibkJobID Int
  deriving (Show, Read, Eq, Ord)

getStarExecId :: JobID -> Int
getStarExecId (StarExecJobID i) = i
getStarExecId _ = error "is no starexec-id!"

getLriId :: JobID -> Int
getLriId (LriJobID t) = t
getLriId _ = error "is no lri-id!"

getUibkId :: JobID -> Int
getUibkId (UibkJobID i) = i
getUibkId _ = error "is no uibk-id"

isStarExecID :: JobID -> Bool
isStarExecID (StarExecJobID _) = True
isStarExecID _ = False

isLriID :: JobID -> Bool
isLriID (LriJobID _) = True
isLriID _ = False

isUibkID :: JobID -> Bool
isUibkID (UibkJobID _) = True
isUibkID _ = False

data SolverID =
  StarExecSolverID Int
  | LriSolverID Int
  | UibkSolverID Int
  deriving (Show, Read, Eq, Ord)

data BenchmarkID =
  StarExecBenchmarkID Int
  | LriBenchmarkID Int
  | UibkBenchmarkID Int
  deriving (Show, Read, Eq, Ord)

--data JobResultID =
--  StarExecResultID Int
--  | LriResultID T.Text
--  deriving (Show, Read, Eq, Ord)

data PostProcID =
  StarExecPostProcID Int
  deriving (Show, Read, Eq, Ord)

data JobPairID =
  StarExecPairID Int
  | LriPairID Int
  | UibkPairID Int
  deriving (Show, Read, Eq, Ord)


getStarExecIds :: JobIds -> [Int]
getStarExecIds = (map getStarExecId) . (filter isStarExecID) . getIds

getLriIds :: JobIds -> [Int]
getLriIds = (map getLriId) . (filter isLriID) . getIds

getUibkIds :: JobIds -> [Int]
getUibkIds = (map getUibkId) . (filter isUibkID) . getIds

-- ReadP als Alternative -> Compilerbau-VO

{-
-}
newtype JobIds = JobIds 
  { getIds :: [JobID]
  }
  deriving (Show, Eq, Read)

readInt :: T.Text -> Maybe Int
readInt t = case reads $ toString t of
  [(i,_)] -> return i
  _       -> Nothing

dePrefix :: T.Text -> T.Text -> T.Text
dePrefix p = T.drop (T.length p)

fromInt :: Int -> T.Text
fromInt = fromString . show

instance PathPiece JobID where
  toPathPiece (StarExecJobID i) = fromInt i
  toPathPiece (LriJobID t) = lriJobPrefix +> (fromString . show) t
  toPathPiece (UibkJobID i) = uibkJobPrefix +> (fromString . show) i
  fromPathPiece t
    | lriJobPrefix `T.isPrefixOf` t =
        case readInt $ dePrefix lriJobPrefix t of
          Just i  -> return $ LriJobID i
          _       -> Nothing
    | uibkJobPrefix `T.isPrefixOf` t =
        case readInt $ dePrefix uibkJobPrefix t of
          Just i  -> return $ UibkJobID i
          _       -> Nothing
    | otherwise =
        case readInt t of
          Just i  -> return $ StarExecJobID i
          _       -> Nothing

--instance PathPiece JobResultID where
--  toPathPiece (StarExecResultID i) = fromInt i
--  toPathPiece (LriResultID t) = lriResultPrefix +> t
--  fromPathPiece t
--    | lriResultPrefix `T.isPrefixOf` t =
--        return $ LriResultID $ dePrefix lriResultPrefix t
--    | otherwise =
--        case readInt t of
--          Just i  -> return $ StarExecResultID i
--          _       -> Nothing

instance PathPiece JobPairID where
  toPathPiece (StarExecPairID i) = fromInt i
  toPathPiece (LriPairID t) = lriPairPrefix +> (fromString . show) t
  toPathPiece (UibkPairID i) = uibkPairPrefix +> (fromString . show) i
  fromPathPiece t
    | lriPairPrefix `T.isPrefixOf` t =
        case readInt $ dePrefix lriPairPrefix t of
          Just i  -> return $ LriPairID i
          _       -> Nothing
    | uibkPairPrefix `T.isPrefixOf` t =
        case readInt $ dePrefix uibkPairPrefix t of
          Just i  -> return $ UibkPairID i
          _       -> Nothing
    | otherwise =
        case readInt t of
          Just i  -> return $ StarExecPairID i
          _       -> Nothing

instance PathPiece BenchmarkID where
  toPathPiece (StarExecBenchmarkID i) = fromInt i
  toPathPiece (LriBenchmarkID t) = lriBenchmarkPrefix +> (fromString . show) t
  toPathPiece (UibkBenchmarkID i) = uibkBenchmarkPrefix +> (fromString . show) i
  fromPathPiece t
    | lriBenchmarkPrefix `T.isPrefixOf` t =
        case readInt $ dePrefix lriBenchmarkPrefix t of
          Just i  -> return $ LriBenchmarkID i
          _       -> Nothing
    | uibkBenchmarkPrefix `T.isPrefixOf` t =
        case readInt $ dePrefix uibkBenchmarkPrefix t of
          Just i  -> return $ UibkBenchmarkID i
          _       -> Nothing
    | otherwise =
        case readInt t of
          Just i  -> return $ StarExecBenchmarkID i
          _       -> Nothing

instance PathPiece SolverID where
  toPathPiece (StarExecSolverID i) = fromInt i
  toPathPiece (LriSolverID t) = lriSolverPrefix +> (fromString . show) t
  toPathPiece (UibkSolverID i) = uibkSolverPrefix +> (fromString . show) i
  fromPathPiece t
    | lriSolverPrefix `T.isPrefixOf` t =
        case readInt $ dePrefix lriSolverPrefix t of
          Just i  -> return $ LriSolverID i
          _       -> Nothing
    | uibkSolverPrefix `T.isPrefixOf` t =
        case readInt $ dePrefix uibkSolverPrefix t of
          Just i  -> return $ UibkSolverID i
          _       -> Nothing
    | otherwise =
        case readInt t of
          Just i  -> return $ StarExecSolverID i
          _       -> Nothing

instance PathPiece PostProcID where
  toPathPiece (StarExecPostProcID i) = fromInt i
  fromPathPiece t
    | otherwise =
        case readInt t of
          Just i  -> return $ StarExecPostProcID i
          _       -> Nothing

instance PathMultiPiece JobIds where
  toPathMultiPiece (JobIds jobs) = toPathMultiPiece $ map toPathPiece $ jobs
  fromPathMultiPiece (j:js) = do
    job <- fromPathPiece j
    (JobIds jobs) <- case js of
                          [] -> return $ JobIds []
                          _ -> fromPathMultiPiece js
    return $ JobIds (job:jobs)
  fromPathMultiPiece _ = Nothing

instance PathPiece Query where
  fromPathPiece "noquery" = return NoQuery
  fromPathPiece t = case reads (T.unpack t) of
    [ (q, "") ] -> return q
    _ -> Nothing
  toPathPiece NoQuery = "noquery"
  toPathPiece q = T.pack $ show q
