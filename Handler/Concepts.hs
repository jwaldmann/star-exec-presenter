module Handler.Concepts where

import Import
import Data.List (elemIndex)
import qualified Data.Set as Set
import Data.Maybe
import Presenter.PersistHelper
import Presenter.Model.Entities()
import ConceptAnalysis.FCA
import ConceptAnalysis.FCAPreparation

-- route to show concepts of given JobID
getConceptsR :: JobID -> Handler Html
getConceptsR jid = do
  jobResults <- getPersistJobResults jid
  let contextData = collectData $ getStarExecResults jobResults
  let context = contextFromList contextData
  let concepts' = concepts context
  defaultLayout $ do
    setTitle "concepts"
    $(widgetFile "concepts")
