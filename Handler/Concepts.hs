module Handler.Concepts where

import FCA.Utils hiding (concepts)
import FCA.StarExec
import FCA.DotGraph (dottedGraph)
import Import
import Presenter.StarExec.JobData
-- import Presenter.StarExec.JobData (queryManyJobs)
-- import Presenter.Utils.WidgetMetaRefresh (insertWidgetMetaRefresh)
import Presenter.Utils.WidgetTable

import Control.Monad
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as Set
import Data.Text as T (isInfixOf)
import Data.Text.Lazy as TL (pack)
import System.Process (readProcess)
import qualified Text.Blaze as B
import Text.Lucius (luciusFile)
import Yesod.Form.Bootstrap3

data AttributeChoices = AttributeChoices
  { chosenSolver :: [Attribute]
  , chosenResults :: Maybe [Attribute]
  , chosenCpu :: Maybe [Attribute]
  , chosenConfig :: Maybe [Attribute]
  }
  deriving (Eq, Show)


-- route with multiselect to choose attributes of JobID
getConceptsR :: ConceptId -> JobIds -> Handler Html
getConceptsR cid jids@(JobIds ids) = do

  attributePairs' <- attributePairs $ getIds jids

  ((result, widget), enctype) <- runFormGet $ renderBootstrap3
    BootstrapBasicForm $ attributeForm $ attrOptionsFromContext $ uniteJobPairAttributes attributePairs'
  let concepts' = case result of
        FormSuccess ca -> do
          let chosenAttributes = filter (not . null) $
                                   (++) [chosenSolver ca] $
                                   map (\f -> maybeListId .f $ ca) [chosenResults, chosenCpu, chosenConfig]
          let filteredPairs = filterPairs attributePairs' chosenAttributes
          case filteredPairs of
            Nothing -> Nothing
            Just fjp -> do
                    let context = contextFromList fjp
                    return $ concepts context
        _ -> Nothing


  let newConcepts = reduceConceptsToProperSubsets concepts' cid
  nodeURLs <- mapM
             (\c -> getConceptURL (fromJust $ elemIndex c $ maybeListId concepts') ids) $
             maybeListId newConcepts
  svgContent <- renderConceptSVG (maybeListId newConcepts) nodeURLs

  let currObjects = maybe Set.empty (\ c -> obs $ c!!cid) concepts'
  qJobs <- queryManyJobs ids
  let filteredJobResults = map
                          ((wrapResults .
                            filter
                            (\jr -> Set.member (getPairID jr) currObjects)
                            . getStarExecResults)
                            . snd . queryResult)
                          qJobs
  tab <- getManyJobCells filteredJobResults

  --actionURL points to concept 0 that shows all objects
  actionURL <- getConceptURL 0 ids
  currURL <- getConceptURL cid ids
  defaultLayout $ do
    -- if any (\q' -> queryStatus q' /= Latest) qJobs
    --   then insertWidgetMetaRefresh
    --   else return ()
    toWidget $(luciusFile "templates/solver_result.lucius")
    setTitle "concepts"
    $(widgetFile "concepts")
    when (isJust concepts') $ displayConcept jids tab


attributeForm :: Map Text [(Text, Attribute)] -> AForm Handler AttributeChoices
attributeForm formOptions = AttributeChoices
  <$> areq (multiSelectFieldList $ fromJust $ M.lookup "Solver name" formOptions) (bfsFormControl MsgSolverNames "SolverNames") Nothing
  <*> aopt (multiSelectFieldList $ fromJust $ M.lookup "Solver config" formOptions) (bfsFormControl MsgSolverConfigs "SolverConfigs") Nothing
  <*> aopt (multiSelectFieldList $ fromJust $ M.lookup "Result" formOptions) (bfsFormControl MsgResults "Results") Nothing
  <*> aopt (multiSelectFieldList $ fromJust $ M.lookup "CPU" formOptions) (bfsFormControl MsgCPUTimes "CPUTimes") Nothing
  <* bootstrapSubmit (BootstrapSubmit {
      bsClasses="btn btn-primary center-block",
      bsValue="choose",
      bsAttrs=[]} :: BootstrapSubmit Text)

bfsFormControl :: RenderMessage master msg => msg -> Text -> FieldSettings master
bfsFormControl msg label = (bfs msg) {fsName = Just label, fsAttrs = [("class", "form-control")]}

attrOptionsFromContext :: Set Attribute -> Map Text [(Text, Attribute)]
attrOptionsFromContext attrs = do
  let allFormOptions = map (\at -> (properAttrName at, at)) $ Set.toList attrs
  let keys = ["Result", "CPU", "Solver config", "Solver name"]
  M.fromList $ map (\key -> (key,
    map (\(label, ats) -> (stripAttributePrefixes label, ats)) $
    filter (\(label, _) -> T.isInfixOf key label) allFormOptions)) keys

getConceptURL :: ConceptId -> [JobID] -> Handler Text
getConceptURL cid jids = do
  rq <- getRequest
  renderer <- getUrlRenderParams
  return $ renderer (ConceptsR cid $ JobIds jids) $ reqGetParams rq

renderConceptSVG :: (Eq ob, Show ob, MonadIO m) => [Concept ob Attribute] -> [Text] -> m B.Markup
renderConceptSVG concepts' nodeURLs = do
  svg <- liftIO $ readProcess "dot" [ "-Tsvg" ] $ dottedGraph concepts' nodeURLs
  -- FIXME: there must be a better way to remove <xml> tag
  return $ B.preEscapedLazyText $ TL.pack $ unlines $ dropWhile ( not . isPrefixOf "<svg" ) $ lines svg

maybeListId :: Maybe [a] -> [a]
maybeListId = fromMaybe []
