module Handler.Concepts where

import FCA.Utils
import FCA.StarExec
import FCA.DotGraph (dottedGraph)
import Import
import Presenter.StarExec.JobData (queryJob)
import Presenter.Utils.WidgetMetaRefresh (insertWidgetMetaRefresh)

import Control.Monad (when)
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import Data.Text as T (isInfixOf)
import Data.Text.Lazy as TL (pack)
import qualified Text.Blaze as B
import System.Process (readProcess)
import Yesod.Form.Bootstrap3


data AttributeChoices = AttributeChoices
  { chosenSolver :: [Attribute]
  , chosenResults :: Maybe [Attribute]
  , chosenCpu :: Maybe [Attribute]
  , chosenConfig :: Maybe [Attribute]
  }
  deriving (Eq)


-- route with multiselect to choose attributes of JobID
getConceptsR :: JobID -> ConceptId-> Handler Html
getConceptsR jid cid = do
  QueryResult qStatus _ <- queryJob jid
  context <- jobResultsContext jid
  let attrs = attributes context
  let options = attrOptionsFromContext attrs
  (widget, enctype) <- generateFormPost $ renderBootstrap3
    (BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 4)) $ attributeForm options
  defaultLayout $ do
    when (qStatus /= Latest)
      -- fetch job from starexec if not present in database
      insertWidgetMetaRefresh
    $(widgetFile "concepts_attributes")


-- route to show concepts of given JobID
postConceptsR :: JobID -> ConceptId-> Handler Html
postConceptsR jid cid = do
  context <- jobResultsContext jid
  let attrs = attributes context
  let options = attrOptionsFromContext attrs
  ((result, _), _) <- runFormPost $ renderBootstrap3
    (BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 4)) $ attributeForm options
  let chosenAttributes = case result of
        FormSuccess ca -> Just ca
        _ -> Nothing

  let solverNames = chosenSolver $ fromJust chosenAttributes
  let newAttributes = Set.fromList $ (++) solverNames $ concat $
                      map (\f -> (maybe [] id) .f $ fromJust chosenAttributes)
                      [chosenResults, chosenCpu, chosenConfig]
  let concepts' = concepts $ filteredContext context newAttributes

  svg <- liftIO $ readProcess "dot" [ "-Tsvg" ] $ dottedGraph concepts'
  -- FIXME: there must be a better way to remove <xml> tag

  let chosenObjects = Set.toList $ obs $ concepts'!!cid
  let svg_contents = B.preEscapedLazyText
                     $ TL.pack $ unlines
                     $ dropWhile ( not . isPrefixOf "<svg" ) $ lines svg
  defaultLayout $ do
    setTitle "concepts"
    $(widgetFile "concepts")


attributeForm :: Map Text [(Text, Attribute)] -> AForm Handler AttributeChoices
attributeForm options =  AttributeChoices
  -- pre-select all options
  -- change widget size to length options
  <$> areq (multiSelectFieldList $ fromJust $ Map.lookup "Solver name" options) "Solver names" Nothing
  <*> aopt (multiSelectFieldList $ fromJust $ Map.lookup "Solver config" options) "Solver configs" Nothing
  <*> aopt (multiSelectFieldList $ fromJust $ Map.lookup "Result" options) "Results" Nothing
  <*> aopt (multiSelectFieldList $ fromJust $ Map.lookup "CPU" options) "CPU times" Nothing
  <* bootstrapSubmit (BootstrapSubmit {
      bsClasses="btn btn-primary",
      bsValue="choose",
      bsAttrs=[("attr-name", "attr-value")]} :: BootstrapSubmit Text)


attrOptionsFromContext :: Set Attribute -> Map Text [(Text, Attribute)]
attrOptionsFromContext attrs = do
  let allOptions = map (\at -> (properAttrName at, at)) $ Set.toList attrs
  let keys = ["Result", "CPU", "Solver config", "Solver name"]
  Map.fromList $ map (\key -> (key, filter (\(label, _) -> T.isInfixOf key label) allOptions)) keys
