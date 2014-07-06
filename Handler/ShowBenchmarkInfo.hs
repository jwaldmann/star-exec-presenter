module Handler.ShowBenchmarkInfo where

import Import
import StarExec.JobData
import StarExec.Types

getShowBenchmarkInfoR :: Int -> Handler Html
getShowBenchmarkInfoR _benchmarkId = do
  (QueryResult qStatus mBenchmarkInfo) <- queryBenchmarkInfo _benchmarkId
  defaultLayout $ do
    $(widgetFile "show_benchmark_info")
