module Handler.LegacyShowBenchmarkInfo where

import Import
import Handler.ShowBenchmarkInfo

getLegacyShowBenchmarkInfoR :: BenchmarkID -> Handler Html
getLegacyShowBenchmarkInfoR = getShowBenchmarkInfoR
