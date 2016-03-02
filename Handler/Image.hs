module Handler.Image where

import FCA.DotGraph (dottedGraph)
import FCA.StarExec
import FCA.Utils
import Import

import System.Process (readProcess)


getImageR :: JobID -> Handler TypedContent
getImageR jid = do
  concepts' <- concepts <$> jobResultsContext jid
  svg_content <- liftIO $ readProcess "dot" ["-Tsvg"] $ dottedGraph concepts' []
  sendResponse $ toTypedContent (typeSvg, toContent svg_content)
