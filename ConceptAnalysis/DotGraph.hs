module ConceptAnalysis.DotGraph where

import Import

import qualified Data.Text.Lazy as TL
import           Data.Graph.Inductive (mkGraph, Gr)
import Data.GraphViz (graphToDot, GraphvizParams, nonClusteredParams)
-- import Data.GraphViz.Types.Graph 
import Data.GraphViz.Printing (renderDot, toDot)
-- import Presenter.PersistHelper
-- import Presenter.Model.Entities()
-- import ConceptAnalysis.FCA
-- import Handler.Concepts
-- import Yesod
-- import Control.Monad (liftM)

-- global_graph_attributes ::
-- global_graph_attributes = 


-- getConcepts :: JobID -> [Concept ob at]
-- getConcepts jid = do
--   jobResults <- getPersistJobResults jid
--   let contextData = collectData $ getStarExecResults jobResults
--   let context = contextFromList contextData
--   concepts context


dotted_graph :: String
dotted_graph = TL.unpack $ renderDot $ toDot $ graphToDot graph_params $ graph
-- renderDot :: DotCode -> Text
-- graphToDot :: (Ord cl, Graph gr) => GraphvizParams Node nl el cl l -> gr nl el -> DotGraph Node

graph :: Gr TL.Text TL.Text
graph = mkGraph [ (2,"one"), (4,"three"), (3,""), (5,"") ] [ (5,4,""), (2,4,"edge label"), (5,2,""), (2,2,""),  (3,2,"") ]
-- mkGraph :: [LNode a] -> [LEdge b] -> gr a b

graph_params :: GraphvizParams n TL.Text TL.Text () TL.Text
graph_params = nonClusteredParams


