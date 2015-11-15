module Handler.Concepts where

import Import
import Data.List (elemIndex)
import qualified Data.Set as Set
import Data.Maybe
import Presenter.PersistHelper
import Presenter.Model.Entities()
import ConceptAnalysis.FCA
import ConceptAnalysis.FCAPreparation
import ConceptAnalysis.DotGraph (dotted_graph)
-- import System.Process (readProcess)
-- import Control.Monad.IO.Class (liftIO)


-- route to show concepts of given JobID
getConceptsR :: JobID -> Handler Html
getConceptsR jid = do
  jobResults <- getPersistJobResults jid
  let contextData = collectData $ getStarExecResults jobResults
  let context = contextFromList contextData
  let concepts' = concepts context

  -- jobResults <- getPersistJobResults 7239
  -- let contextData = collectData $ getStarExecResults jobResults
  -- let context = contextFromList contextData
  -- let concepts' = concepts context
  -- let svg = getImage
  defaultLayout $ do
    setTitle "concepts"
    $(widgetFile "concepts")

-- getImage :: MonadHandler m => m TypedContent
-- getImage = do
--   -- jobResults <- getPersistJobResults 7239
--   -- let contextData = collectData $ getStarExecResults jobResults
--   -- let context = contextFromList contextData
--   -- let concepts' = concepts context
--   svg_content <- liftIO $ readProcess "dot" [ "-Tsvg", "-Gsize=10,100" ] dotted_graph
--   sendResponse $ toTypedContent (typeSvg, toContent svg_content)
