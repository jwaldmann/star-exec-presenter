module Handler.Image where

import Import

import System.Process (readProcess)
import Control.Monad.IO.Class (liftIO)
import ConceptAnalysis.DotGraph (dotted_graph)

getImageR :: MonadHandler m => m TypedContent
getImageR = do
  svg_content <- liftIO $ readProcess "dot" [ "-Tsvg", "-Gsize=10,100" ] dotted_graph
  sendResponse $ toTypedContent (typeSvg, toContent svg_content)
