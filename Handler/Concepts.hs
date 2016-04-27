module Handler.Concepts where

import FCA.Utils hiding (concepts)
import FCA.StarExec
import FCA.DotGraph (dottedGraph)
import Import
import Presenter.PersistHelper
import Presenter.Processing
import Presenter.Short
import Presenter.StarExec.JobData (queryJob)
import Presenter.Utils.WidgetMetaRefresh (insertWidgetMetaRefresh)

import Control.Monad (when)
import Data.Double.Conversion.Text
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as Set
import Data.Text as T (append, isInfixOf, takeEnd)
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
getConceptsR :: JobID -> ConceptId -> Handler Html
getConceptsR jid cid = do
  QueryResult qStatus _ <- queryJob jid
  context <- jobResultsContext jid
  let attrs = attributes context
  ((result, widget), enctype) <- runFormGet $ renderBootstrap3
    BootstrapBasicForm $ attributeForm $ attrOptionsFromContext attrs
  let chosenAttributes = case result of
        FormSuccess ca -> do
          let solvers = chosenSolver ca
          filter ((not . null)) $ (++) [solvers] $
                       map (\f -> maybeListId .f $ ca)
                       [chosenResults, chosenCpu, chosenConfig]
        _ -> [[]]

  let filtered_context = filterContext context chosenAttributes
  let concepts' = case filtered_context of
                    Nothing -> Nothing
                    Just fc -> Just $ concepts fc

  jobResults <- mapM (\obj -> getPersistJobResult obj) $ Set.toList $ maybe Set.empty (\c -> obs $ c!!cid) concepts'
  let jobSolvers = Set.fromList $ map (\jr -> (jid, getSolver jr)) $ catMaybes jobResults
  let benchmarkResults = getBenchmarkRows (catMaybes jobResults) jobSolvers

  nodeURLs <- mapM (\c -> getConceptURL jid (fromJust $ elemIndex c $ maybeListId concepts')) $ maybeListId concepts'
  svg_contents <- renderConceptSVG (maybeListId concepts') $ addTableAnchor nodeURLs

-- actionURL points to concept 0 that shows all objects
  actionURL <- getConceptURL jid 0
  currURL <- getConceptURL jid cid
  defaultLayout $ do
    when (qStatus /= Latest)
       -- fetch job from starexec if not present in database
       insertWidgetMetaRefresh
    toWidget $(luciusFile "templates/solver_result.lucius")
    setTitle "concepts"
    $(widgetFile "concepts")


attributeForm :: Map Text [(Text, Attribute)] -> AForm Handler AttributeChoices
attributeForm formOptions = AttributeChoices

  -- change widget size to length of respective option
  <$> areq (multiSelectFieldList $ fromJust $ M.lookup "Solver name" formOptions) (bfsFormControl MsgSolverNames "SolverNames") Nothing
  <*> aopt (multiSelectFieldList $ fromJust $ M.lookup "Solver config" formOptions) (bfsFormControl MsgSolverConfigs "SolverConfigs") Nothing
  <*> aopt (multiSelectFieldList $ fromJust $ M.lookup "Result" formOptions) (bfsFormControl MsgResults "Results") Nothing
  <*> aopt (multiSelectFieldList $ fromJust $ M.lookup "CPU" formOptions) (bfsFormControl MsgCPUTimes "CPUTimes") Nothing
  <* bootstrapSubmit (BootstrapSubmit {
      bsClasses="btn btn-primary",
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

resultmap :: [JobResult] -> Map UniqueBenchmark (Map (JobID, (SolverID, SolverName)) JobResult)
resultmap jobResults = M.fromListWith M.union $ do
  jr <- jobResults
  return ( getBenchmark jr, M.singleton (getJobID jr, getSolver jr) jr )

getBenchmarkRows :: [JobResult] -> Set (JobID, (SolverID, SolverName)) -> [(UniqueBenchmark, [Maybe JobResult])]
getBenchmarkRows jobResults jobSolvers = do
    (bm, rowmap) <- M.toList $ resultmap jobResults
    let row = do
          s <- Set.toList jobSolvers
          return $ M.lookup s rowmap
    return (bm, row)

shorten :: Text -> Text
shorten = T.takeEnd 50

addTableAnchor :: [Text] ->  [Text]
addTableAnchor = map (\nodeURL -> append nodeURL "#result-table")

maybeListId :: Maybe [a] -> [a]
maybeListId = maybe [] id
