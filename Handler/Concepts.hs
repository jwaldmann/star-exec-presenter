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


data AttributeChoice = AttributeChoice
  { chosenResults :: [Attribute]
  , chosenCpu :: [Attribute]
  , chosenSolver :: [Attribute]
  , chosenConfig :: [Attribute]
  }
  deriving (Eq)


-- route with multiselect to choose attributes of JobID
getConceptsR :: JobID -> Handler Html
getConceptsR jid =  do
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
postConceptsR :: JobID -> Handler Html
postConceptsR jid = do
  context <- jobResultsContext jid
  let attrs = attributes context
  let options = attrOptionsFromContext attrs
  ((result, _), _) <- runFormPost $ renderBootstrap3 
    (BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 4)) $ attributeForm options
  let chosenAttributes = case result of
        FormSuccess ca -> Just ca
        _ -> Nothing

  -- not very beautiful, use applicative instead
  let newAttributes = Set.fromList $ (chosenResults $ fromJust chosenAttributes)
                                  ++ (chosenCpu $ fromJust chosenAttributes)
                                  ++ (chosenSolver $ fromJust chosenAttributes)
                                  ++ (chosenConfig $ fromJust chosenAttributes)
  let concepts' = concepts $ filteredContext context newAttributes

  svg <- liftIO $ readProcess "dot" [ "-Tsvg" ] $ dottedGraph concepts'
  -- FIXME: there must be a better way to remove <xml> tag
  let svg_contents = B.preEscapedLazyText
                     $ TL.pack $ unlines
                     $ dropWhile ( not . isPrefixOf "<svg" ) $ lines svg
  defaultLayout $ do
    setTitle "concepts"
    $(widgetFile "concepts")


attributeForm :: Map Text [(Text, Attribute)] -> AForm Handler AttributeChoice
attributeForm options =  AttributeChoice
  -- pre-select all options
  -- change widget size to length options
  <$> areq (multiSelectFieldList $ fromJust $ Map.lookup "Result" options) "Results" Nothing
  <*> areq (multiSelectFieldList $ fromJust $ Map.lookup "CPU" options) "CPU times" Nothing
  <*> areq (multiSelectFieldList $ fromJust $ Map.lookup "Solver name" options) "Solver names" Nothing
  <*> areq (multiSelectFieldList $ fromJust $ Map.lookup "Solver config" options) "Solver configs" Nothing
  <* bootstrapSubmit (BootstrapSubmit {
      bsClasses="btn btn-primary",
      bsValue="choose",
      bsAttrs=[("attr-name", "attr-value")]} :: BootstrapSubmit Text)
  

attrOptionsFromContext :: Set Attribute -> Map Text [(Text, Attribute)]
attrOptionsFromContext attrs = do
  let allOptions = map (\at -> (properAttrName at, at)) $ Set.toList attrs
  let ll = Map.singleton "Result" (filter (\(label, _) -> T.isInfixOf "Result" label) allOptions)
  let lll = Map.insert "CPU" (filter (\(label, _) -> T.isInfixOf "CPU" label) allOptions) ll
  let llll = Map.insert "Solver config" (filter (\(label, _) -> T.isInfixOf "Solver config" label) allOptions) lll
  Map.insert "Solver name" (filter (\(label, _) -> T.isInfixOf "Solver name" label) allOptions) llll
  -- let options = map (\key -> (key, filter (\(label, _) -> T.isInfixOf key label) allOptions)) ["Result", "CPU", "Solver conifg", "Solver name"]
  -- Map.fromList options
