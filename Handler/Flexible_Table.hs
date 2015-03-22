module Handler.Flexible_Table where

import Import

import StarExec.Types 
import StarExec.JobData ( queryManyJobs )
import Table.Data
import Table.Query
import Table.Get
import Utils.WidgetMetaRefresh

import Text.Lucius (luciusFile)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Function (on)
import Data.List (sortBy, inits, tails, isPrefixOf)

import qualified Data.Graph.Inductive as G
import qualified Data.GraphViz as V
import qualified Data.GraphViz.Printing as V
import qualified Data.GraphViz.Attributes as V
import qualified Data.GraphViz.Attributes.Complete as V
import qualified System.Process as P
import qualified Data.Text.Lazy as T
import qualified Text.Blaze as B
import Control.Monad ( guard )
import Data.Maybe ( isJust, maybeToList )

getFlexible_TableR :: Query -> JobIds -> Handler Html
getFlexible_TableR q @ (Query ts) jids @ (JobIds ids) = do
  qJobs <- queryManyJobs ids
  tab <- Table.Get.getManyJobCells $ map (snd . queryResult) qJobs
  defaultLayout $ do
    setTitle "Flexible Table"
    toWidget $(luciusFile "templates/solver_result.lucius")
    if any (\q -> queryStatus q /= Latest) qJobs
      then insertWidgetMetaRefresh
      else return ()
    [whamlet|
            <pre>#{show q}
        |]
    display jids [] ts tab 

-- display :: [Transform] -> Table -> Handler Html
display jids previous ts tab  = do
  summary jids previous tab
  [whamlet| 
      <a href=@{Flexible_TableR (Query []) jids}>remove following #{show (length ts)} transformations
  |]
  case ts of
    (t:later) -> do

        [whamlet|
            <h3>apply transformation
               <pre>#{show t}
        |]

        display jids (previous ++ [t]) later $ apply t tab
        
    [] -> do
    -- no more transformers, display actual data
        let rs = rows tab
        [whamlet|
             <h3>data
             <table class="table">
              <thead>
                <tr>             
                    $forall h <- header tab
                        <th> ^{contents h}
              <tbody>
                    $forall row <- rs
                        <tr> 
                          $forall cell <- row
                            <td class="#{tdclass cell}"> ^{contents cell} 
            |]

supertypes :: [a] -> [[Maybe a]]
supertypes xs = sequence $ map ( \ x -> [Just x, Nothing] ) xs

predecessors :: [Maybe a] -> [[Maybe a]]
predecessors xs = do
  (pre, Just x : post) <- splits xs
  return $ pre ++ Nothing : post

splits xs = zip (inits xs) (tails xs)

summary jids previous tab = do
    let total = length $ rows tab
        column_stats = M.fromListWith (+) $ do
            row <- rows tab
            (i,cell) <- zip [0..] row
            return ((tag cell, i),1)        
        tags = S.fromList $ map fst $ M.keys column_stats         
        width = length (header tab) 
        column_stats_table = for (S.toList tags) $ \ t ->          
            for [0 .. width - 1 ] $ \ i -> 
                let n = M.findWithDefault 0 (t, i) column_stats 
                    (pre, _ : post) = splitAt i $ replicate width Any
                    p = And $ pre ++ Equals t : post
                    these = Query $ previous ++ [ Filter_Rows p ]
                    others =  Query $ previous ++ [ Filter_Rows (Not p) ]
                in  (t,i,n, (these, others))
        positive n = n > 0
        row_type_stats = M.fromListWith (+) $ do
            row <- rows tab
            return (map tag row, 1)
        row_type_table = for ( sortBy (compare `on` snd) $ M.toList row_type_stats ) 
            $ \ (rt, n) -> 
                (rt, n, Query (previous ++ [ Filter_Rows (And (map Equals rt)) ] ) 
                      , Query (previous ++ [ Filter_Rows (Not (And (map Equals rt))) ] ) 
                )
        concept_stats = M.fromListWith (+) $ do
          row <- rows tab
          concept <- supertypes $ map tag row
          -- next line is hack (1st column is benchmark with attribute "nothing")
          guard $ case concept of Nothing : _ -> False ; _ -> True
          return (concept, 1)
        concept_table = for ( sortBy (compare `on` snd) $ M.toList concept_stats )
          $ \ (c, n) ->
            let f = And $ for c $ \ x ->
                      case x of Nothing -> Any ; Just t -> Equals t
            in  (c, n , Query (previous ++ [ Filter_Rows f ] )
                      , Query (previous ++ [ Filter_Rows $ Not f ] )
                )
        -- http://hackage.haskell.org/package/fgl-5.5.1.0/docs/Data-Graph-Inductive-Graph.html#t:LNode
        nodes = M.fromList $ zip [ 1 .. ] (M.keys concept_stats) 
        inodes = M.fromList $ zip (M.keys concept_stats) [ 1.. ]
        concept_graph :: G.Gr [Maybe Text] ()
        concept_graph = G.mkGraph (M.toList nodes) $ do
          (k,p) <- M.toList nodes
          q <- predecessors p
          i <- maybeToList $ M.lookup q inodes
          return (i, k, () )
        -- cf.   http://stackoverflow.com/a/20860364/2868481
        dot =  V.renderDot $ V.toDot
            $ V.graphToDot
               V.nonClusteredParams
                { V.fmtNode = \ (n,l) ->
                   [ V.toLabel $ show $ concept_stats M.! l
                   , V.Tooltip $ T.pack $ show l
                   , V.URL [shamlet|@{Flexible_TableR (Query previous) jids}|] 
                   ]
                }
            $ concept_graph
    -- FIXME: this uses String, but it should use Text:        
    svg <- liftIO $ P.readProcess "dot" [ "-Tsvg", "-Gsize=10,100" ] $ T.unpack dot
    -- FIXME: there must be a better way
    let svg_contents = B.preEscapedLazyText
                     $ T.pack $ unlines
                     $ dropWhile ( not . isPrefixOf "<svg" ) $ lines svg
    [whamlet|
        <h3>summary
        total number of rows: #{show total}
        <h3>columns, by tags
        <table class="table">
          <thead>
           <tr>             
             $forall h <- header tab
                <th> ^{contents h}
          <tbody>
             $forall r <- column_stats_table
               <tr>
                 $forall (t,i,n, (these, others)) <- r
                    $if positive n
                        <td class="#{t}"> 
                          #{t} #{show n}
                          <a href=@{Flexible_TableR these jids}>these
                          | <a href=@{Flexible_TableR others jids}>others
                    $else 
                        <td>
        <h3>row types
        <table class="table">
          <thead>
           <tr>             
             $forall h <- header tab
                <th> ^{contents h}
          <tbody>
            $forall (rt, n, these,others) <- row_type_table
              <tr>
                $forall t <- rt
                    <td class="#{t}"> #{t}
                <td> #{show n}
                <td> 
                   <a href=@{Flexible_TableR these jids}>these
                <td>
                   <a href=@{Flexible_TableR others jids}>others
        <h3>concepts (partial row types)        
        <div>
          #{svg_contents}
        <table class="table">
          <thead>
           <tr>             
             $forall h <- header tab
                <th> ^{contents h}
          <tbody>
            $forall (rt, n, these,others) <- concept_table
              <tr>
                $forall mt <- rt
                    $maybe t <- mt
                        <td class="#{t}"> #{t}
                    $nothing
                        <td>
                <td> #{show n}
                <td> 
                   <a href=@{Flexible_TableR these jids}>these
                <td>
                   <a href=@{Flexible_TableR others jids}>others
    |]
    
apply t tab = case t of
    Filter_Rows p -> 
            tab { rows = filter ( \ row -> predicate p $ map tag row ) $ rows tab }

predicate p tags = case p of
    And fs -> and $ zipWith matches fs tags
    Not q -> not $ predicate q tags

matches f c = case f of
    Any -> True
    Equals s -> s == c
    Not_Equals s -> s /= c
