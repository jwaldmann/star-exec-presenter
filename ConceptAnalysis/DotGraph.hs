module ConceptAnalysis.DotGraph where

import Import

import Control.Monad (guard)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as TL
import           Data.Graph.Inductive (mkGraph, Gr, LNode, LEdge)
import Data.GraphViz (graphToDot, GraphvizParams, nonClusteredParams)
import Data.GraphViz.Algorithms (transitiveReduction)
import Data.GraphViz.Printing (renderDot, toDot)
import ConceptAnalysis.FCA
import ConceptAnalysis.FCAPreparation
import Data.Set (fromList, showTree, isProperSubsetOf)

-- global_graph_attributes ::
-- global_graph_attributes = 


dotted_graph :: [Concept JobPairId Attribute] -> String
dotted_graph concept_lattice = do
  let graph_with_trans_edges = graphToDot graph_params $ graph concept_lattice
  TL.unpack $ renderDot $ toDot $ transitiveReduction graph_with_trans_edges
-- renderDot :: DotCode -> Text
-- graphToDot :: (Ord cl, Graph gr) => GraphvizParams Node nl el cl l -> gr nl el -> DotGraph Node

graph :: (Eq ob, Eq at, Show at, Ord at) => [Concept ob at] -> Gr TL.Text TL.Text
graph concept_lattice = do
  mkGraph (get_nodes concept_lattice) $ get_edges concept_lattice

get_nodes :: (Eq ob, Eq at, Show at) => [Concept ob at] -> [LNode TL.Text]
get_nodes concept_lattice = map
 (\c -> (fromJust $ elemIndex c concept_lattice, TL.pack $ showTree $ ats c))
 concept_lattice

get_edges :: (Eq ob, Eq at, Ord at) => [Concept ob at] -> [LEdge TL.Text]
get_edges concept_lattice = do
  concept <- concept_lattice
  concept2 <- concept_lattice
  guard (isProperSubsetOf (ats concept) (ats concept2))
  -- math: ats concept < ats concept2 -> (ats concept) -> (ats concept2)
  return (fromJust $ elemIndex concept concept_lattice, fromJust $ elemIndex concept2 concept_lattice, "")

graph_params :: GraphvizParams n TL.Text TL.Text () TL.Text
graph_params = nonClusteredParams
