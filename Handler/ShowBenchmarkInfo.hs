module Handler.ShowBenchmarkInfo where

import Import
import Presenter.StarExec.JobData
import Presenter.Internal.Stringish
import Presenter.Utils.WidgetMetaRefresh
import Presenter.PersistHelper

getBenchmarkType :: Benchmark -> Text
getBenchmarkType (StarExecBenchmark b) = benchmarkInfoType b
getBenchmarkType _ = notAvailable

getLastUpdate :: Benchmark -> Text
getLastUpdate (StarExecBenchmark b) = fromString $ show $ benchmarkInfoLastUpdate b
getLastUpdate _ = notAvailable

getFile :: Benchmark -> Text
getFile (LriBenchmark b) = lriBenchmarkInfoFile b
getFile _ = notAvailable

getRating :: Benchmark -> Text
getRating (LriBenchmark b) = fromString $ show $ lriBenchmarkInfoRating b
getRating _ = notAvailable

getSolved :: Benchmark -> Text
getSolved (LriBenchmark b) = fromString $ show $ lriBenchmarkInfoSolved b
getSolved _ = notAvailable

isConditional :: Benchmark -> Text
isConditional (LriBenchmark b) = fromBool $ lriBenchmarkInfoIsConditional b
isConditional _ = no

isContextSensitive :: Benchmark -> Text
isContextSensitive (LriBenchmark b) = fromBool $ lriBenchmarkInfoIsContextSensitive b
isContextSensitive _ = no

isInnermost :: Benchmark -> Text
isInnermost (LriBenchmark b) = fromBool $ lriBenchmarkInfoIsInnermost b
isInnermost _ = no

isOutermost :: Benchmark -> Text
isOutermost (LriBenchmark b) = fromBool $ lriBenchmarkInfoIsOutermost b
isOutermost _ = no

isRelative :: Benchmark -> Text
isRelative (LriBenchmark b) = fromBool $ lriBenchmarkInfoIsRelative b
isRelative _ = no

isTheory :: Benchmark -> Text
isTheory (LriBenchmark b) = fromBool $ lriBenchmarkInfoIsTheory b
isTheory _ = no

getShowBenchmarkInfoR :: BenchmarkID -> Handler Html
getShowBenchmarkInfoR bid@(StarExecBenchmarkID _id) = do
  (QueryResult qStatus mBenchmarkInfo) <- queryBenchmarkInfo bid
  defaultLayout $ do
    case qStatus of
      Latest -> return ()
      Pending _ -> insertWidgetMetaRefresh
    $(widgetFile "se_show_benchmark_info")
getShowBenchmarkInfoR bid@(LriBenchmarkID _id) = do
  mBenchmarkInfo <- getPersistBenchmarkInfo bid
  defaultLayout $ do
    $(widgetFile "lri_show_benchmark_info")

