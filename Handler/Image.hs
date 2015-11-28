module Handler.Image where

import Import

import System.Process (readProcess)
import Control.Monad.IO.Class (liftIO)
import ConceptAnalysis.DotGraph (dottedGraph)
import Presenter.PersistHelper
import Presenter.Model.Entities()
import ConceptAnalysis.FCA
import ConceptAnalysis.FCAPreparation
import Handler.Concepts


getImageR :: MonadHandler Handler => JobID -> Handler TypedContent
getImageR jid = do
  jobResults <- getPersistJobResults jid
  let contextData = collectData $ getStarExecResults jobResults
  let context = contextFromList contextData
  let concepts' = concepts context
  svg_content <- liftIO $ readProcess "dot" [ "-Tsvg", "-Gsize=10,100" ] $ dottedGraph concepts'
  sendResponse $ toTypedContent (typeSvg, toContent svg_content)
