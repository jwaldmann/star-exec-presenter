module Handler.RenderBenchmark where

import Import
import Data.Text.Encoding
import Presenter.StarExec.JobData
import Presenter.StarExec.Commands (getBenchmark)
import Presenter.StarExec.Connection (getConnection)
import qualified Data.ByteString.Lazy as BSL
import Handler.DisplayProof (getFile)

getRenderBenchmarkR :: BenchmarkID ->  Handler TypedContent
getRenderBenchmarkR (StarExecBenchmarkID bmId) = do
  con <- getConnection
  cont <- getBenchmark con bmId
  let contents = decodeUtf8 $ BSL.toStrict cont
  return $ toTypedContent $ repXml cont

getRenderXmlR :: Text -> Handler TypedContent
getRenderXmlR f = do
  getFile f
  
