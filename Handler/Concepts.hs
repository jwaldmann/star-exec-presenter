module Handler.Concepts where

import FCA.Utils
import FCA.StarExec
import FCA.DotGraph (dottedGraph)
import Import
-- import Presenter.StarExec.JobData (queryJob)
-- import Presenter.Utils.WidgetMetaRefresh (insertWidgetMetaRefresh)

-- import Control.Monad (when)
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
  deriving (Eq, Show)


-- route with multiselect to choose attributes of JobID
getConceptsR :: JobID -> ConceptId -> Handler Html
getConceptsR = postConceptsR

-- route to show concepts of given JobID
postConceptsR :: JobID -> ConceptId -> Handler Html
postConceptsR jid cid = do
  context <- jobResultsContext jid
  let attrs = attributes context
  ((result, widget), enctype) <- runFormGet $ renderBootstrap3
    (BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 4)) $ attributeForm $ attrOptionsFromContext attrs
  let attributeChoices = case result of
        FormSuccess ca -> Just ca
        _ -> Just AttributeChoices {chosenSolver=[], chosenResults=Just [], chosenCpu=Just [], chosenConfig=Just []}
  let solverNames = chosenSolver $ fromJust attributeChoices
  let chosenAttributes = Set.fromList $ (++) solverNames $ concat $
                      map (\f -> (maybe [] id) .f $ fromJust attributeChoices)
                      [chosenResults, chosenCpu, chosenConfig]
  let concepts' = concepts $ filteredContext context chosenAttributes
  let chosenObjects = Set.toList $ obs $ concepts'!!cid

  -- actionURL points to concept 0 that shows all objects
  actionURL <- getConceptURL jid 0
  currURL <- getConceptURL jid cid
  nodeURLs <- mapM (\c -> getConceptURL jid (fromJust $ elemIndex c concepts')) concepts'

  svg_contents <- renderConceptSVG concepts' nodeURLs
  defaultLayout $ do
    setTitle "concepts"
    $(widgetFile "concepts")


attributeForm :: Map Text [(Text, Attribute)] -> AForm Handler AttributeChoices
attributeForm formOptions =  AttributeChoices
  -- change widget size to length of respective option
  <$> areq (multiSelectFieldList $ fromJust $ Map.lookup "Solver name" formOptions) "Solver names" Nothing
  <*> aopt (multiSelectFieldList $ fromJust $ Map.lookup "Solver config" formOptions) "Solver configs" Nothing
  <*> aopt (multiSelectFieldList $ fromJust $ Map.lookup "Result" formOptions) "Results" Nothing
  <*> aopt (multiSelectFieldList $ fromJust $ Map.lookup "CPU" formOptions) "CPU times" Nothing
  <* bootstrapSubmit (BootstrapSubmit {
      bsClasses="btn btn-primary",
      bsValue="choose",
      bsAttrs=[("attr-name", "attr-value")]} :: BootstrapSubmit Text)


attrOptionsFromContext :: Set Attribute -> Map Text [(Text, Attribute)]
attrOptionsFromContext attrs = do
  let allFormOptions = map (\at -> (properAttrName at, at)) $ Set.toList attrs
  let keys = ["Result", "CPU", "Solver config", "Solver name"]
  Map.fromList $ map (\key -> (key, filter (\(label, _) -> T.isInfixOf key label) allFormOptions)) keys


getConceptURL :: JobID -> ConceptId -> Handler Text
getConceptURL jid cid = do
  rq <- getRequest
  let getParams = reqGetParams rq
  renderer <- getUrlRenderParams
  return $ (renderer $ ConceptsR (jid) cid) getParams


renderConceptSVG :: (Eq ob, Show ob, MonadIO m) => [Concept ob Attribute] -> [Text] -> m B.Markup
renderConceptSVG concepts' nodeURLs = do
  svg <- liftIO $ readProcess "dot" [ "-Tsvg" ] $ dottedGraph concepts' nodeURLs
  -- FIXME: there must be a better way to remove <xml> tag
  return $ B.preEscapedLazyText $ TL.pack $ unlines $ dropWhile ( not . isPrefixOf "<svg" ) $ lines svg
