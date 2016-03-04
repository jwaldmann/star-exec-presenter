module Handler.Concepts where

import FCA.Utils
import FCA.StarExec
import FCA.DotGraph (dottedGraph)
import Import
import Presenter.PersistHelper
import Presenter.Processing
import Presenter.Short
-- import Presenter.StarExec.JobData (queryJob)
-- import Presenter.Utils.WidgetMetaRefresh (insertWidgetMetaRefresh)

-- import Control.Monad (when)
import Data.Double.Conversion.Text
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as Set
import Data.Text as T (isInfixOf, takeEnd)
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
  let solvers = chosenSolver $ fromJust attributeChoices
  let chosenAttributes = Set.fromList $ (++) solvers $ concat $
                      map (\f -> (maybe [] id) .f $ fromJust attributeChoices)
                      [chosenResults, chosenCpu, chosenConfig]
  let concepts' = concepts $ filterContext context chosenAttributes
  let chosenObjects = Set.toList $ obs $ concepts'!!cid

  jobResults <- mapM (\obj -> getPersistJobResult obj) chosenObjects
  let jobSolvers = Set.fromList $ map (\jr -> (jid, getSolver jr)) $ catMaybes jobResults
  let benchmarkResults = getBenchmarkRows (catMaybes jobResults) jobSolvers
  --let rm = resultmap jobResults


  -- actionURL points to concept 0 that shows all objects
  actionURL <- getConceptURL jid 0
  currURL <- getConceptURL jid cid
  nodeURLs <- mapM (\c -> getConceptURL jid (fromJust $ elemIndex c concepts')) concepts'
  svg_contents <- renderConceptSVG concepts' nodeURLs
  defaultLayout $ do
    toWidget $(luciusFile "templates/solver_result.lucius")
    setTitle "concepts"
    $(widgetFile "concepts")


attributeForm :: Map Text [(Text, Attribute)] -> AForm Handler AttributeChoices
attributeForm formOptions =  AttributeChoices
  -- change widget size to length of respective option
  <$> areq (multiSelectFieldList $ fromJust $ M.lookup "Solver name" formOptions) "Solver names" Nothing
  <*> aopt (multiSelectFieldList $ fromJust $ M.lookup "Solver config" formOptions) "Solver configs" Nothing
  <*> aopt (multiSelectFieldList $ fromJust $ M.lookup "Result" formOptions) "Results" Nothing
  <*> aopt (multiSelectFieldList $ fromJust $ M.lookup "CPU" formOptions) "CPU times" Nothing
  <* bootstrapSubmit (BootstrapSubmit {
      bsClasses="btn btn-primary",
      bsValue="choose",
      bsAttrs=[("attr-name", "attr-value")]} :: BootstrapSubmit Text)


attrOptionsFromContext :: Set Attribute -> Map Text [(Text, Attribute)]
attrOptionsFromContext attrs = do
  let allFormOptions = map (\at -> (properAttrName at, at)) $ Set.toList attrs
  let keys = ["Result", "CPU", "Solver config", "Solver name"]
  M.fromList $ map (\key -> (key, filter (\(label, _) -> T.isInfixOf key label) allFormOptions)) keys


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
shorten t = T.takeEnd 50 t
