module Handler.Image where

import Import

import System.Process (readProcess)
import Control.Monad.IO.Class (liftIO)
import ConceptAnalysis.DotGraph (dotted_graph)
-- import Presenter.PersistHelper
--import Presenter.Model.Entities()
--import ConceptAnalysis.FCA
--import Handler.Concepts


getImageR :: MonadHandler m => m TypedContent
getImageR = do
  -- jobResults <- getPersistJobResults 7239
  -- let contextData = collectData $ getStarExecResults jobResults
  -- let context = contextFromList contextData
  -- let concepts' = concepts context
  svg_content <- liftIO $ readProcess "dot" [ "-Tsvg", "-Gsize=10,100" ] dotted_graph
  sendResponse $ toTypedContent (typeSvg, toContent svg_content)
