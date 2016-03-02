module FCA.DotGraph where

import FCA.Utils
import FCA.StarExec as FSE
import Import hiding (delete)
import Presenter.Utils.Colors as C

import Control.Monad
import Data.Graph.Inductive hiding (getNodes)
import Data.GraphViz as G
import Data.GraphViz.Algorithms
import Data.GraphViz.Attributes.Complete as GA
import Data.GraphViz.Attributes.HTML as GAH
import Data.GraphViz.Printing
import Data.List as L
import Data.Maybe
import Data.Set as S
import Data.Text as T hiding (length, map)
import qualified Data.Text.Lazy as TL


dottedGraph :: (Eq ob, Show ob) => [Concept ob FSE.Attribute] -> [T.Text] -> String
dottedGraph concept_lattice nodeUrls = do
  let graph_params = getGraphParams concept_lattice nodeUrls
  let graph_with_trans_edges = G.graphToDot graph_params $ createGraph concept_lattice
  TL.unpack $ renderDot $ toDot $ transitiveReduction graph_with_trans_edges

createGraph :: (Eq ob, Eq at, Show at, Ord at) => [Concept ob at] -> (Gr TL.Text TL.Text)
createGraph concept_lattice = mkGraph (getNodes concept_lattice) $ getEdges concept_lattice

getNodes :: (Eq ob, Eq at, Show at) => [Concept ob at] -> [LNode TL.Text]
getNodes concept_lattice = L.map
 (\c -> (fromJust $ elemIndex c concept_lattice, "")) concept_lattice

getEdges :: (Eq ob, Eq at, Ord at) => [Concept ob at] -> [LEdge TL.Text]
getEdges concept_lattice = do
  concept <- concept_lattice
  concept2 <- concept_lattice
  guard (isProperSubsetOf (ats concept) (ats concept2))
  -- math: ats concept < ats concept2 --> Edge((ats concept), (ats concept2))
  return (fromJust $ elemIndex concept concept_lattice, fromJust $ elemIndex concept2 concept_lattice, "")

getGraphParams :: (Integral n, Show n) => [Concept ob FSE.Attribute] -> [T.Text] -> G.GraphvizParams n TL.Text TL.Text () TL.Text
getGraphParams concept_lattice nodeUrls = G.nonClusteredParams {
   G.globalAttributes = [G.GraphAttrs
                          [
                            GA.RankDir GA.FromLeft,
                            GA.Size GA.GSize {width=15, height=Nothing, desiredSize=False},
                            GA.Tooltip " "
                          ]
                        ]
   , G.isDirected       = True
   , G.fmtNode          = \ (n, _) -> do
     let concept = concept_lattice!!(fromIntegral n)
     let (atLabels,nodeColor) = replaceLabelWithColor $ S.map properAttrName $ ats concept

     -- https://hackage.haskell.org/package/graphviz-2999.18.0.2/docs/Data-GraphViz-Attributes-HTML.html#t:Table
     [
       GA.Shape GA.PlainText, GA.Label $ GA.HtmlLabel $ GAH.Table $ GAH.HTable Nothing [ GAH.CellBorder 0, GAH.BGColor nodeColor] [
       -- first row:
       GAH.Cells [GAH.LabelCell [HRef $ TL.fromStrict $ nodeUrls!!(fromIntegral n)] $ GAH.Text [htmlTextItemWrapper n]],
       -- second row:
       GAH.Cells [GAH.LabelCell [] $ GAH.Text [htmlTextItemWrapper $ length $ obs concept]],
       -- third row:
       GAH.Cells [GAH.LabelCell [] $ GAH.Text [htmlTextItemWrapper $ toList atLabels]]
      ]]
   }

getSolverResultColor :: T.Text -> Color
getSolverResultColor solverResults
    | containsYes solverResults = C.colorYes
    | containsNo solverResults = C.colorNo
    | containsMaybe solverResults  = C.colorMaybe
    | containsBounds solverResults  = C.colorBounds
    | containsCertified solverResults  = C.colorCertified
    | containsError solverResults  = C.colorError
    | otherwise = C.colorAnything
    where
      containsYes = T.isInfixOf "YES"
      containsNo = T.isInfixOf "NO"
      containsMaybe = T.isInfixOf "MAYBE"
      containsBounds = T.isInfixOf "BOUNDS"
      containsCertified = T.isInfixOf "CERTIFIED"
      containsError = T.isInfixOf "ERROR"


replaceLabelWithColor :: Set T.Text -> (Set T.Text, Color)
replaceLabelWithColor labels = do
  let solverResults = S.filter (\at -> "Result " `T.isPrefixOf` at) labels
  if length solverResults == 1
    then
      (difference labels solverResults, getSolverResultColor $ S.elemAt 0 solverResults)
    else (difference labels solverResults, toColor G.White)


htmlTextItemWrapper :: Show a => a -> TextItem
htmlTextItemWrapper string = GAH.Str $ TL.pack $ show string
