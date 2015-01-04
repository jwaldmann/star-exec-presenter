module Presenter.Model.RouteTypes where

import Prelude
import Yesod
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Presenter.Internal.Stringish
import Presenter.Model.Query
import Presenter.Internal.Stringish

lriResultPrefix :: T.Text
lriResultPrefix = "lri_result."

lriJobPrefix :: T.Text
lriJobPrefix = "lri_job."

lriPairPrefix :: T.Text
lriPairPrefix = "lri_pair."

lriSolverPrefix :: T.Text
lriSolverPrefix = "lri_solver."

lriBenchmarkPrefix :: T.Text
lriBenchmarkPrefix = "lri_benchmark."

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
  | LriJobID T.Text
  deriving (Show, Read, Eq, Ord)

getStarExecId :: JobID -> Int
getStarExecId (StarExecJobID i) = i
getStarExecId _ = error "is no starexec-id!"

getLriId :: JobID -> T.Text
getLriId (LriJobID t) = t
getLriId _ = error "is no lri-id!"

isStarExecID :: JobID -> Bool
isStarExecID (StarExecJobID _) = True
isStarExecID _ = False

isLriID :: JobID -> Bool
isLriID (LriJobID _) = True
isLriID _ = False

data SolverID =
  StarExecSolverID Int
  | LriSolverID T.Text
  deriving (Show, Read, Eq, Ord)

data BenchmarkID =
  StarExecBenchmarkID Int
  | LriBenchmarkID T.Text
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
  | LriPairID T.Text
  deriving (Show, Read, Eq, Ord)


getStarExecIds :: JobIds -> [Int]
getStarExecIds = (map getStarExecId) . (filter isStarExecID) . getIds

getLriIds :: JobIds -> [T.Text]
getLriIds = (map getLriId) . (filter isLriID) . getIds

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
  toPathPiece (LriJobID t) = lriJobPrefix +> t
  fromPathPiece t
    | lriJobPrefix `T.isPrefixOf` t =
        return $ LriJobID $ dePrefix lriJobPrefix t
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
  toPathPiece (LriPairID t) = lriPairPrefix +> t
  fromPathPiece t
    | lriPairPrefix `T.isPrefixOf` t =
        return $ LriPairID $ dePrefix lriPairPrefix t
    | otherwise =
        case readInt t of
          Just i  -> return $ StarExecPairID i
          _       -> Nothing

instance PathPiece BenchmarkID where
  toPathPiece (StarExecBenchmarkID i) = fromInt i
  toPathPiece (LriBenchmarkID t) = lriBenchmarkPrefix +> t
  fromPathPiece t
    | lriBenchmarkPrefix `T.isPrefixOf` t =
        return $ LriBenchmarkID $ dePrefix lriBenchmarkPrefix t
    | otherwise =
        case readInt t of
          Just i  -> return $ StarExecBenchmarkID i
          _       -> Nothing

instance PathPiece SolverID where
  toPathPiece (StarExecSolverID i) = fromInt i
  toPathPiece (LriSolverID t) = lriSolverPrefix +> t
  fromPathPiece t
    | lriSolverPrefix `T.isPrefixOf` t =
        return $ LriSolverID $ dePrefix lriSolverPrefix t
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
