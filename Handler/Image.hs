module Handler.Image where

import Import

import System.Process (readProcess)
import ConceptAnalysis.DotGraph (dottedGraph)
import Presenter.PersistHelper
import Presenter.Model.Entities()
import ConceptAnalysis.FCA
import ConceptAnalysis.FCAPreparation


getImageR :: JobID -> Handler TypedContent
getImageR jid = do
  jobResults <- getPersistJobResults jid
  let contextData = collectData $ getStarExecResults jobResults
  let context = contextFromList contextData
  let concepts' = concepts context
  svg_content <- liftIO $ readProcess "dot" ["-Tsvg"] $ dottedGraph concepts'
  sendResponse $ toTypedContent (typeSvg, toContent svg_content)
