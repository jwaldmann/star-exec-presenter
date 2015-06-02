module Handler.ListBenchmarks where

import Import
import Presenter.PersistHelper

-- Pagination is probably needed!

getAll :: Handler [Benchmark]
getAll = runDB $ do
  starExecBenchmarks <- do
    benchmarks <- getEntityList' ([] :: [Filter BenchmarkInfo]) []
    return $ StarExecBenchmark <$> benchmarks
  lriBenchmarks <- do
    benchmarks <- getEntityList' ([] :: [Filter LriBenchmarkInfo]) []
    return $ LriBenchmark <$> benchmarks
  return $ starExecBenchmarks ++ lriBenchmarks

getListBenchmarksR :: Handler Html
getListBenchmarksR = do
  benchmarks <- getAll
  defaultLayout $ do
    $(widgetFile "list_benchmarks")
