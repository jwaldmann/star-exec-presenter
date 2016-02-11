module Handler.Concepts where

import Import
-- import Presenter.StarExec.JobData (queryJob)
-- import Presenter.Utils.WidgetMetaRefresh (insertWidgetMetaRefresh)
import FCA.Utils
import FCA.StarExec

import Data.List (elemIndex)
import Data.Maybe
--import Control.Monad (when)
import           Data.Set (Set)
import qualified Data.Set as Set
import Yesod.Form.Bootstrap3


data AttributeChoice = AttributeChoice
  { attributeSet :: [Attribute] }
  deriving (Eq)


-- route to multiselect with attributes of JobID
getConceptsR :: JobID -> Handler Html
getConceptsR jid =  do
  context <- jobResultsContext jid
  let attrs = attributes context
  let options = attrOptionsFromContext attrs
  (widget, enctype) <- generateFormPost $ attributeForm options
  defaultLayout $(widgetFile "concepts_attributes")


-- route to show concepts of given JobID
postConceptsR :: JobID -> Handler Html
postConceptsR jid = do
  --QueryResult qStatus _ <- queryJob jid
  context <- jobResultsContext jid
  let attrs = attributes context
  let options = attrOptionsFromContext attrs
  ((result, _), _) <- runFormPost $ attributeForm options
  let chosenAttributes = case result of
        FormSuccess ca -> Just ca
        _ -> Nothing
  let newAttributes = Set.fromList $ attributeSet $ fromJust chosenAttributes
  -- if old attributeSet == new attributeSet do nothing

  let concepts' = concepts $ filteredContext context newAttributes
  defaultLayout $ do
    -- fetch job from starexec if not present in database
    -- when (qStatus /= Latest)
    --  insertWidgetMetaRefresh
    setTitle "concepts"
    $(widgetFile "concepts")


attributeForm :: [(Text, Attribute)] -> Form AttributeChoice
attributeForm options = renderBootstrap3 BootstrapBasicForm $ AttributeChoice
  -- pre-select all options
  -- change widget size to length options
  <$> areq (multiSelectFieldList options) "chooseAttributes" Nothing
  --where attributes = [("1"::Text, (AJobResultInfoSolver "woohoo"))]


attrOptionsFromContext :: Set Attribute -> [(Text, Attribute)]
attrOptionsFromContext attrs = do
  map (\at -> (properAttrName at, at)) $ Set.toList attrs
