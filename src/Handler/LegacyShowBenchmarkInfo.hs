module Handler.LegacyShowBenchmarkInfo where

import Import
import Network.HTTP.Types.Status

getLegacyShowBenchmarkInfoR :: BenchmarkID -> Handler Html
getLegacyShowBenchmarkInfoR = (redirectWith movedPermanently301) . ShowBenchmarkInfoR
