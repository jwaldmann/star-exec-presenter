module Handler.Concepts where

import FCA.Basic hiding (concepts)
import FCA.StarExec
import FCA.DotGraph (renderConceptSVG)
import FCA.Helpers
import Import
import Presenter.StarExec.JobData
-- import Presenter.Utils.WidgetMetaRefresh (insertWidgetMetaRefresh)
import Presenter.Utils.WidgetTable

import Control.Monad
import Data.List (elemIndex)
import Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as Set
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
getConceptsR :: ConceptId -> ComplementIds -> JobIds -> Handler Html
getConceptsR cid compls@(Ids complIds) jids@(JobIds ids) = do
  qJobs <- queryManyJobs ids

  attributePairs' <- attributePairs $ fmap (snd . queryResult) qJobs

  ((result, widget), enctype) <- ((runFormGet . renderBootstrap3 BootstrapBasicForm) . attributeForm ) .
    attrOptions $ uniteJobPairAttributes attributePairs'
  let concepts' = case result of
        FormSuccess ca -> do
          let chosenAttributes = filter (not . null) .
                                   (++) [chosenSolver ca] $
                                   map (\f -> maybeListId .f $ ca) [chosenResults, chosenCpu, chosenConfig]
          let filteredPairs = filterPairsByAttributes attributePairs' chosenAttributes
          case filteredPairs of
            Nothing -> Nothing
            Just pairs -> do
                    return . concepts $ contextFromList $ reducePairsByComplements pairs $ Ids complIds
        _ -> Nothing


  let newConcepts = reduceConceptsToProperSubsets concepts' cid

  nodeURLs <- mapM
             (\c -> getConceptURL (fromJust . elemIndex c $ maybeListId concepts') compls ids) $
             maybeListId newConcepts
  complURLs <- mapM
              (\c -> getConceptURL cid (Ids (complIds ++ [fromJust . elemIndex c $ maybeListId concepts'])) ids) $
              maybeListId newConcepts

  svgContent <- renderConceptSVG (maybeListId newConcepts) nodeURLs complURLs

  let currObjects = maybe
                    Set.empty
                    (\ c -> do
                      let concept = safeGetIndex c cid
                      case concept of
                        Nothing -> Set.empty
                        Just c'  -> obs c')
                    concepts'

  let filteredJobResults = fmap
                          ((wrapResults .
                            filter
                            (\jr -> Set.member (getPairID jr) currObjects)
                            . getStarExecResults)
                            . snd . queryResult)
                          qJobs
  tab <- getManyJobCells filteredJobResults

  --actionURL points to concept 0 that shows all objects
  actionURL <- getConceptURL 0 compls ids
  currURL <- getConceptURL cid compls ids
  resetComplURL <- getConceptURL cid (Ids []) ids
  defaultLayout $ do
    -- when (any (\q' -> queryStatus q' /= Latest) qJobs ) insertWidgetMetaRefresh
    toWidget $(luciusFile "templates/solver_result.lucius")
    setTitle "concepts"
    $(widgetFile "concepts")
    when (isJust concepts') $ displayConcept jids tab


attributeForm :: Map Text [(Text, Attribute)] -> AForm Handler AttributeChoices
attributeForm formOptions = AttributeChoices
  <$> areq (multiSelectFieldList . fromJust $ M.lookup "SolverYearName" formOptions) (bfsFormControl MsgSolverNames "SolverNames") Nothing
  <*> aopt (multiSelectFieldList . fromJust $ M.lookup "Solver config" formOptions) (bfsFormControl MsgSolverConfigs "SolverConfigs") Nothing
  <*> aopt (multiSelectFieldList . fromJust $ M.lookup "Result" formOptions) (bfsFormControl MsgResults "Results") Nothing
  <*> aopt (multiSelectFieldList . fromJust $ M.lookup "CPU" formOptions) (bfsFormControl MsgCPUTimes "CPUTimes") Nothing
  <* bootstrapSubmit (BootstrapSubmit {
      bsClasses="btn btn-primary center-block",
      bsValue="choose",
      bsAttrs=[]} :: BootstrapSubmit Text)

bfsFormControl :: RenderMessage master msg => msg -> Text -> FieldSettings master
bfsFormControl msg label = (bfs msg) {fsName = Just label, fsAttrs = [("class", "form-control")]}

-- create attribute form field options
attrOptions :: Set Attribute -> Map Text [(Text, Attribute)]
attrOptions attrs = do
  M.fromList $ fmap
               (\(fieldName, atrPred) -> (fieldName, filter (\(_, atr) ->  atrPred atr) . fmap (\atr -> (properAttrName atr, atr)) $ Set.toList attrs))
               [("Result",isASolverResult)
               ,("CPU",isASlowCpuTime)
               ,("Solver config"
               ,isAJobResultInfoConfiguration)
               ,("SolverYearName",isAYearSpecificSolverName)]

getConceptURL :: ConceptId -> ComplementIds -> [JobID] -> Handler Text
getConceptURL cid compls jids = do
  rq <- getRequest
  renderer <- getUrlRenderParams
  return . renderer (ConceptsR cid compls $ JobIds jids) $ reqGetParams rq
