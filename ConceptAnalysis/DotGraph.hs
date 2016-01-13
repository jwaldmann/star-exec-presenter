module ConceptAnalysis.DotGraph where

import Import

import Control.Monad (guard)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as TL
import           Data.Graph.Inductive (mkGraph, Gr, LNode, LEdge)
import qualified Data.GraphViz as G
import Data.GraphViz.Algorithms (transitiveReduction)
import Data.GraphViz.Printing (renderDot, toDot)
import Data.GraphViz.Attributes.Complete as GA
import Data.GraphViz.Attributes.HTML as GAH
import ConceptAnalysis.FCA
import Data.Set (isProperSubsetOf, toList)


dottedGraph :: (Eq ob, Eq at, Show ob, Show at, Ord at) => [Concept ob at] -> String
dottedGraph concept_lattice = do
  let graph_params = getGraphParams concept_lattice
  let graph_with_trans_edges = G.graphToDot graph_params $ createGraph concept_lattice
  TL.unpack $ renderDot $ toDot $ transitiveReduction graph_with_trans_edges

createGraph :: (Eq ob, Eq at, Show at, Ord at) => [Concept ob at] -> (Gr TL.Text TL.Text)
createGraph concept_lattice = mkGraph (getNodes concept_lattice) $ getEdges concept_lattice

getNodes :: (Eq ob, Eq at, Show at) => [Concept ob at] -> [LNode TL.Text]
getNodes concept_lattice = map
 (\c -> (fromJust $ elemIndex c concept_lattice, "")) concept_lattice

getEdges :: (Eq ob, Eq at, Ord at) => [Concept ob at] -> [LEdge TL.Text]
getEdges concept_lattice = do
  concept <- concept_lattice
  concept2 <- concept_lattice
  guard (isProperSubsetOf (ats concept) (ats concept2))
  -- math: ats concept < ats concept2 -> (ats concept) -> (ats concept2)
  return (fromJust $ elemIndex concept concept_lattice, fromJust $ elemIndex concept2 concept_lattice, "")

getGraphParams :: (Integral n, Show n, Show at, Show ob) => [Concept ob at] -> G.GraphvizParams n TL.Text TL.Text () TL.Text
getGraphParams concept_lattice = G.Params {
    G.isDirected       = True
   , G.globalAttributes = [G.GraphAttrs [GA.RankDir GA.FromBottom]]
   , G.clusterBy        = G.N
   , G.isDotCluster     = const True
   , G.clusterID        = const (G.Num $ G.Int 0)
   , G.fmtCluster       = const []
   ,G.fmtNode           = \ (n, _) -> do
     let concept = concept_lattice!!(fromIntegral n)
     -- https://hackage.haskell.org/package/graphviz-2999.18.0.2/docs/Data-GraphViz-Attributes-HTML.html#t:Table
     [
       GA.Shape GA.PlainText, GA.Label $ GA.HtmlLabel $ GAH.Table $ GAH.HTable Nothing [ GAH.CellBorder 0] [
       -- first row:
       GAH.Cells [GAH.LabelCell [HRef "http://example.com"] $ GAH.Text [GAH.Str $ TL.pack $ show n]],
       -- second row:
       GAH.Cells [GAH.LabelCell [] $ GAH.Text [GAH.Str $ TL.pack $ show $ length $ obs concept]],
       -- third row:
       GAH.Cells [GAH.LabelCell [] $ GAH.Text [GAH.Str $ TL.pack $ show $ toList $ ats concept]]
      ]]
   , G.fmtEdge          = const []
   }
