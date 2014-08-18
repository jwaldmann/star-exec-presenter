module Presenter.RouteTypes where

import Prelude
import Yesod
import qualified Data.Text as T
import qualified Data.Text.Read as TR

{-
-}
data ErrorID = LoginError | Unkown
  deriving (Eq, Show, Read)

{-
-}
data JobID =
  StarExecJobID Int
  | LriJobID T.Text
  deriving (Show, Eq, Read, Ord)

isStarExecID :: JobID -> Bool
isStarExecID (StarExecJobID _) = True
isStarExecID _ = False

getStarExecId :: JobID -> Int
getStarExecId (StarExecJobID i) = i
getStarExecId _ = error "is no starexec-id!"

getLriId :: JobID -> T.Text
getLriId (LriJobID t) = t
getLriId _ = error "is no lri-id!"

isLriID :: JobID -> Bool
isLriID (LriJobID _) = True
isLriID _ = False

data SolverID =
  StarExecSolverID Int
  | LriSolverID T.Text
  deriving (Show, Eq, Read, Ord)

data BenchmarkID =
  StarExecBenchmarkID Int
  | LriBenchmarkID T.Text
  deriving (Show, Eq, Read, Ord)

data JobResultID =
  StarExecResultID Int
  | LriResultID T.Text
  deriving (Show, Eq, Read, Ord)

data JobPairID =
  StarExecPairID Int
  | LriPairID T.Text
  deriving (Show, Eq, Read, Ord)


getStarExecIds :: JobIds -> [Int]
getStarExecIds = (map getStarExecId) . (filter isStarExecID) . getIds

getLriIds :: JobIds -> [T.Text]
getLriIds = (map getLriId) . (filter isLriID) . getIds

{-
-}
newtype JobIds = JobIds 
  { getIds :: [JobID]
  }
  deriving (Show, Eq, Read)

instance PathPiece JobID where
  toPathPiece (StarExecJobID i) = toPathPiece i
  toPathPiece (LriJobID t) = toPathPiece ("lri." `T.append` t)
  fromPathPiece t
    | "lri." `T.isPrefixOf` t = return $ LriJobID $ T.drop 4 t
    | otherwise               =
      case TR.decimal t of
        Right (i,_) -> return $ StarExecJobID i
        Left _      -> Nothing

instance PathPiece BenchmarkID where
  toPathPiece (StarExecBenchmarkID i) = toPathPiece i
  toPathPiece (LriBenchmarkID t) = toPathPiece t
  fromPathPiece t
    | "lri." `T.isPrefixOf` t = return $ LriBenchmarkID $ T.drop 4 t
    | otherwise               =
      case TR.decimal t of
        Right (i,_) -> return $ StarExecBenchmarkID i
        Left _      -> Nothing

instance PathPiece SolverID where
  toPathPiece (StarExecSolverID i) = toPathPiece i
  toPathPiece (LriSolverID t) = toPathPiece t
  fromPathPiece t
    | "lri." `T.isPrefixOf` t = return $ LriSolverID $ T.drop 4 t
    | otherwise               =
      case TR.decimal t of
        Right (i,_) -> return $ StarExecSolverID i
        Left _      -> Nothing

instance PathMultiPiece JobIds where
  toPathMultiPiece (JobIds ints) = toPathMultiPiece $ map show $ ints
  fromPathMultiPiece (i:is) = do
    int <- fromPathPiece i
    (JobIds ints) <- case is of
                          [] -> return $ JobIds []
                          _ -> fromPathMultiPiece is
    return $ JobIds (int:ints)
  fromPathMultiPiece _ = Nothing
