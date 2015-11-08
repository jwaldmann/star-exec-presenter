module Handler.Image where

import Import

import System.Process (readProcess)
import Control.Monad.IO.Class (liftIO)


getImageR :: MonadHandler m => m TypedContent
getImageR = do
  svg_content <- liftIO $ readProcess "dot" [ "-Tsvg", "-Gsize=10,100" ] "digraph {1; 3; 1 -> 3;}"
  sendResponse $ toTypedContent (typeSvg, toContent svg_content)
