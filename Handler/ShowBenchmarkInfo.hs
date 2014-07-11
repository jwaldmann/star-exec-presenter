module Handler.ShowBenchmarkInfo where

import Import
import StarExec.JobData
import StarExec.Types
import Utils.WidgetMetaRefresh

getShowBenchmarkInfoR :: Int -> Handler Html
getShowBenchmarkInfoR _benchmarkId = do
  (QueryResult qStatus mBenchmarkInfo) <- queryBenchmarkInfo _benchmarkId
  defaultLayout $ do
    case qStatus of
      Latest -> return ()
      Pending _ -> insertWidgetMetaRefresh
    $(widgetFile "show_benchmark_info")
