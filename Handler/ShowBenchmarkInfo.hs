module Handler.ShowBenchmarkInfo where

import Import
import StarExec.Connection
import StarExec.Commands

getShowBenchmarkInfoR :: Int -> Handler Html
getShowBenchmarkInfoR _benchmarkId = do
  con <- getConnection
  mBenchmarkInfo <- getBenchmarkInfo con _benchmarkId
  defaultLayout $ do
    $(widgetFile "show_benchmark_info")
