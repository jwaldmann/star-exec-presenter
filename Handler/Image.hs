module Handler.Image where

import FCA.DotGraph (dottedGraph)
import FCA.Utils
import FCA.StarExec
import Import
import Presenter.PersistHelper
import Presenter.Model.Entities()

import System.Process (readProcess)


getImageR :: JobID -> Handler TypedContent
getImageR jid = do
  jobResults <- getPersistJobResults jid
  let contextData = collectData $ getStarExecResults jobResults
  let context = contextFromList contextData
  let concepts' = concepts context
  svg_content <- liftIO $ readProcess "dot" ["-Tsvg"] $ dottedGraph concepts'
  sendResponse $ toTypedContent (typeSvg, toContent svg_content)
