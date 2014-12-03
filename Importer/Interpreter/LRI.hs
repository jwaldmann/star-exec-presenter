module Importer.Interpreter.LRI 
  ( getBenchmarks
  , getSolvers
  , getResults
  ) where
  
import Prelude
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as Char8
import Importer.Internal
import Control.Applicative
import Control.Monad

isSolver :: Content -> Bool
isSolver = any (\(k,_) -> k == "url")

isBenchmark :: Content -> Bool
isBenchmark = any (\(k,_) -> k == "file")

findKey :: Content -> Name -> Maybe (Key, Value)
findKey kvs key = L.find ((==key) . fst) kvs

removeQuotes :: Value -> Value
removeQuotes v =
  if "\"" `BSL.isPrefixOf` v && "\"" `BSL.isSuffixOf` v
    then Char8.drop 1 $ Char8.take (BSL.length v - 1) v
    else v

readName :: Content -> Name -> Either String Name
readName kvs key =
  case findKey kvs key of
    Just (_,v) -> return $ removeQuotes v
    Nothing    -> Left $ "key not found: " ++ (show key)

readInt :: Content -> Name -> Either String Int
readInt kvs key =
  case findKey kvs key of
    Just (_,v) ->
      case reads $ Char8.unpack v of
        [(i,_)] -> return i
        _       -> Left $ "couldn't parse value " ++ (show v) ++ "for key " ++ (show key)
    Nothing    -> return (-1)
    --Nothing    -> Left $ "key not found: " ++ (show key)

readBool :: Content -> Name -> Either String Bool
readBool kvs key =
  case findKey kvs key of
    Just (_,"false") -> return False
    Just (_,"true")  -> return True
    Just (_,_)       -> return False
    Nothing          -> return False

readPairResult :: Content -> Key -> Either String LRIPairResult
readPairResult kvs key = do
  result <- readPairResultMaybe kvs key
  case result of
    Just r  -> return r
    Nothing -> Left $ "key not found: " ++ (show key)

readPairResultMaybe :: Content -> Key -> Either String (Maybe LRIPairResult)
readPairResultMaybe kvs key =
  case findKey kvs key of
    Just (_,bs)    -> 
      case removeQuotes bs of
        "YES"       -> return $ Just LRIYES
        "NO"        -> return $ Just LRINO
        "ERROR"     -> return $ Just LRIERROR
        "DONT_KNOW" -> return $ Just LRIMAYBE
        "TIMEOUT"   -> return $ Just LRITIMEOUT
        bs'         -> return $ Just $ LRIOTHER bs'
    Nothing        -> return Nothing

readDouble :: Content -> Key -> Either String Double
readDouble kvs key = do
  result <- readDoubleMaybe kvs key
  case result of
    Just r  -> return r
    Nothing -> Left $ "key not found: " ++ (show key)

readDoubleMaybe :: Content -> Key -> Either String (Maybe Double)
readDoubleMaybe kvs key =
  case findKey kvs key of
    Just (_,s) ->
      case reads $ Char8.unpack $ removeQuotes s of
        [(d,_)] -> return $ Just d
        _       -> return Nothing
    Nothing    -> return Nothing

readBenchmark :: Entry -> Either String LRIBenchmark
readBenchmark (name, kvs) = do
  let readName' = readName kvs
      readInt' = readInt kvs
      readBool' = readBool kvs
  LRIBenchmark name
    <$> readName' "name"
    <*> readName' "file"
    <*> readInt' "rating"
    <*> readInt' "solved"
    <*> readBool' "conditional"
    <*> readBool' "contextsensitive"
    <*> readBool' "innermost"
    <*> readBool' "outermost"
    <*> readBool' "relative"
    <*> readBool' "theory"

readSolver :: Entry -> Either String LRISolver
readSolver (name, kvs) = do
  let readName' = readName kvs
      readBool' = readBool kvs
  LRISolver name
    <$> readName' "name"
    <*> readName' "author"
    <*> readName' "description"
    <*> readName' "url"
    <*> readBool' "standard"
    <*> readBool' "relative"
    <*> readBool' "conditional"
    <*> readBool' "contextsensitive"
    <*> readBool' "innermost"
    <*> readBool' "theory"
    <*> readBool' "certifying"

interpret :: [Entry] -> Either String ([LRIBenchmark], [LRISolver])
interpret entries = do
  interpretedBenchmarks <- getBenchmarks entries
  interpretedSolvers <- getSolvers entries
  return (interpretedBenchmarks, interpretedSolvers)

getBenchmarks :: [Entry] -> Either String [LRIBenchmark]
getBenchmarks entries = do
  let benchmarks = filter (isBenchmark . snd) entries
  mapM readBenchmark benchmarks

getSolvers :: [Entry] -> Either String [LRISolver]
getSolvers entries = do
  let solvers = filter (isSolver . snd) entries
  mapM readSolver solvers

getResults :: [Entry] -> Either String [LRIResult]
getResults entries = do
  let benchmarks = filter (isBenchmark . snd) entries
  foldM extractResults [] benchmarks
  where
    extractResults :: [LRIResult] -> Entry -> Either String [LRIResult]
    extractResults extracted (name, content) = do
      let resultKVs = filter ((Char8.any (=='_')) . fst) content
          groupedBySolver = L.groupBy match resultKVs
      results <- mapM (parseResult name) groupedBySolver
      return $ results ++ extracted
    match :: (Key, Value) -> (Key, Value) -> Bool
    match (key1,_) (key2,_) = let k1 = Char8.takeWhile (/='_') key1
                                  k2 = Char8.takeWhile (/='_') key2
                              in k1 == k2
    parseResult :: Name -> Content -> Either String LRIResult
    parseResult benchmarkKey [] = Left $ "benchmark-content incomplete? " ++ (show benchmarkKey)
    parseResult benchmarkKey kvs@(c:_) = do
      let solverKey = extractSolverKey c
          (+>) = BSL.append
      LRIResult (benchmarkKey +> "+" +> solverKey) benchmarkKey solverKey
        <$> readPairResult kvs (solverKey +> "_res_120")
        <*> readDouble kvs (solverKey +> "_time_120")
        <*> readDouble kvs (solverKey +> "_wctime_120")
        <*> readPairResultMaybe kvs (solverKey +> "_check_res")
        <*> readDoubleMaybe kvs (solverKey +> "_check_time")
        <*> readDoubleMaybe kvs (solverKey +> "_check_wctime")
    extractSolverKey :: (Key, Value) -> Key
    extractSolverKey (k,_) = Char8.takeWhile (/='_') k
