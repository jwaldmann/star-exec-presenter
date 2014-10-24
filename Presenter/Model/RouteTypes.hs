module Presenter.Model.RouteTypes where

import Prelude
import Yesod
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Presenter.Internal.Stringish
import Presenter.Model.Query
import Presenter.Internal.Stringish

openingParenthese :: Char
openingParenthese = '('

closingParenthese :: Char
closingParenthese = ')'

lriResultPrefix :: String
lriResultPrefix = "lri_result."

lriJobPrefix :: String
lriJobPrefix = "lri_job."

lriPairPrefix :: String
lriPairPrefix = "lri_pair."

lriSolverPrefix :: String
lriSolverPrefix = "lri_solver."

lriBenchmarkPrefix :: String
lriBenchmarkPrefix = "lri_benchmark."

class ShowID a where
  showID :: a -> String

class ReadID a where
  readsPrecID :: ReadS a

showID_ :: String -> Char -> String -> Char -> String
showID_ prefix p1 s p2 = prefix ++ [p1] ++ s ++ [p2]

showID_' :: String -> T.Text -> String
showID_' prefix t = showID_ prefix openingParenthese (toString t) closingParenthese

lexID :: Char -> Char -> ReadS String
lexID p1 p2 s = lexID_ s
  where
    lexID_ (x:xs) = if x == p1
                      then lexID_' ("", xs) 1
                      else []
    lexID_ _      = []
    lexID_' (v,(x:xs)) n
      | x == p1           = lexID_' (v ++ [p1], xs) (n+1)
      | x == p2 && n == 1 = [(v,xs)]
      | x == p2           = lexID_' (v ++ [p2], xs) (n-1)
      | otherwise         = lexID_' (v ++ [x], xs) n

lexID' :: ReadS String
lexID' = lexID openingParenthese closingParenthese

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
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

data BenchmarkID =
  StarExecBenchmarkID Int
  | LriBenchmarkID T.Text
  deriving (Eq, Ord)

data JobResultID =
  StarExecResultID Int
  | LriResultID T.Text
  deriving (Eq, Ord)

data JobPairID =
  StarExecPairID Int
  | LriPairID T.Text
  deriving (Eq, Ord)


getStarExecIds :: JobIds -> [Int]
getStarExecIds = (map getStarExecId) . (filter isStarExecID) . getIds

getLriIds :: JobIds -> [T.Text]
getLriIds = (map getLriId) . (filter isLriID) . getIds

instance ShowID JobID where
  showID (StarExecJobID i) = show i
  showID (LriJobID t) = showID_' lriJobPrefix t

instance ReadID JobID where
  readsPrecID s = [(LriJobID (fromString t), w) | ("lri_job", u) <- lex s,
                                                  (".", v)          <- lex u,
                                                  (t, w)            <- lexID openingParenthese closingParenthese v ]
                  ++
                  [(StarExecJobID i, x) | (i, x) <- reads s ]

instance Show JobID where
  show = showID

instance Read JobID where
  readsPrec _ = readsPrecID



instance ShowID SolverID where
  showID (StarExecSolverID i) = show i
  showID (LriSolverID t) = showID_ lriSolverPrefix openingParenthese (toString t) closingParenthese

instance ReadID SolverID where
  readsPrecID s = [(LriSolverID (fromString t), w) | ("lri_solver", u) <- lex s,
                                                     (".", v)          <- lex u,
                                                     (t, w)            <- lexID openingParenthese closingParenthese v ]
                  ++
                  [(StarExecSolverID i, x) | (i, x) <- reads s ]

instance Show SolverID where
  show = showID

instance Read SolverID where
  readsPrec _ = readsPrecID



instance ShowID BenchmarkID where
  showID (StarExecBenchmarkID i) = show i
  showID (LriBenchmarkID t) = showID_ lriBenchmarkPrefix openingParenthese (toString t) closingParenthese

instance ReadID BenchmarkID where
  readsPrecID s = [(LriBenchmarkID (fromString t), w) | ("lri_benchmark", u) <- lex s,
                                                        (".", v)             <- lex u,
                                                        (t, w)               <- lexID openingParenthese closingParenthese v ]
                  ++
                  [(StarExecBenchmarkID i, x) | (i, x) <- reads s ]

instance Show BenchmarkID where
  show = showID

instance Read BenchmarkID where
  readsPrec _ = readsPrecID



instance ShowID JobResultID where
  showID (StarExecResultID i) = show i
  showID (LriResultID t) = showID_ lriResultPrefix openingParenthese (toString t) closingParenthese

instance ReadID JobResultID where
  readsPrecID s = [(LriResultID (fromString t), w) | ("lri_result", u) <- lex s,
                                                     (".", v)          <- lex u,
                                                     (t, w)            <- lexID openingParenthese closingParenthese v ]
                  ++
                  [(StarExecResultID i, x) | (i, x) <- reads s ]

instance Show JobResultID where
  show = showID

instance Read JobResultID where
  readsPrec _ = readsPrecID



instance ShowID JobPairID where
  showID (StarExecPairID i) = show i
  showID (LriPairID t) = showID_ lriPairPrefix openingParenthese (toString t) closingParenthese

instance ReadID JobPairID where
  readsPrecID s = [(LriPairID (fromString t), w) | ("lri_pair", u) <- lex s,
                                                   (".", v)          <- lex u,
                                                   (t, w)            <- lexID openingParenthese closingParenthese v ]
                  ++
                  [(StarExecPairID i, x) | (i, x) <- reads s ]

instance Show JobPairID where
  show = showID

instance Read JobPairID where
  readsPrec _ = readsPrecID



{-
-}
newtype JobIds = JobIds 
  { getIds :: [JobID]
  }
  deriving (Show, Eq, Read)

instance PathPiece JobID where
  toPathPiece = fromString . show
  --toPathPiece (StarExecJobID i) = toPathPiece i
  --toPathPiece (LriJobID t) = toPathPiece (lriJobPrefix +> t)
  fromPathPiece t = case reads $ toString t of
    [(i,_)] -> return i
    _       -> Nothing
  --fromPathPiece t
  --  | lriJobPrefix `T.isPrefixOf` t = return $ LriJobID $ T.drop 4 t
  --  | otherwise =
  --    case TR.decimal t of
  --      Right (i,_) -> return $ StarExecJobID i
  --      Left _      -> Nothing

instance PathPiece JobResultID where
  toPathPiece = fromString . show
  --toPathPiece (StarExecResultID i) = toPathPiece i
  --toPathPiece (LriResultID t) = toPathPiece (lriResultPrefix +> t)
  fromPathPiece t = case reads $ toString t of
    [(i,_)] -> return i
    _       -> Nothing
  --fromPathPiece t
  --  | lriResultPrefix `T.isPrefixOf` t = return $ LriResultID $ T.drop 4 t
  --  | otherwise =
  --    case TR.decimal t of
  --      Right (i,_) -> return $ StarExecResultID i
  --      Left _      -> Nothing

instance PathPiece JobPairID where
  toPathPiece = fromString . show
  --toPathPiece (StarExecPairID i) = toPathPiece i
  --toPathPiece (LriPairID t) = toPathPiece (lriPairPrefix +> t)
  fromPathPiece t = case reads $ toString t of
    [(i,_)] -> return i
    _       -> Nothing
  --fromPathPiece t
  --  | lriPairPrefix `T.isPrefixOf` t = return $ LriPairID $ T.drop 4 t
  --  | otherwise =
  --    case TR.decimal t of
  --      Right (i,_) -> return $ StarExecPairID i
  --      Left _      -> Nothing

instance PathPiece BenchmarkID where
  toPathPiece = fromString . show
  --toPathPiece (StarExecBenchmarkID i) = toPathPiece i
  --toPathPiece (LriBenchmarkID t) = toPathPiece (lriBenchmarkPrefix +> t)
  fromPathPiece t = case reads $ toString t of
    [(i,_)] -> return i
    _       -> Nothing
  --fromPathPiece t
  --  | lriBenchmarkPrefix `T.isPrefixOf` t = return $ LriBenchmarkID $ T.drop 4 t
  --  | otherwise =
  --    case TR.decimal t of
  --      Right (i,_) -> return $ StarExecBenchmarkID i
  --      Left _      -> Nothing

instance PathPiece SolverID where
  toPathPiece = fromString . show
  --toPathPiece (StarExecSolverID i) = toPathPiece i
  --toPathPiece (LriSolverID t) = toPathPiece t
  fromPathPiece t = case reads $ toString t of
    [(i,_)] -> return i
    _       -> Nothing
  --fromPathPiece t
  --  | "lri_solver." `T.isPrefixOf` t = return $ LriSolverID $ T.drop 4 t
  --  | otherwise =
  --    case TR.decimal t of
  --      Right (i,_) -> return $ StarExecSolverID i
  --      Left _      -> Nothing

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

