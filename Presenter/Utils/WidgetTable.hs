module Presenter.Utils.WidgetTable where

import Import
import Text.Blaze (ToMarkup)

import Presenter.Processing ( getClass )
import Presenter.Model.Additional.Table
import Presenter.Internal.Stringish

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Function (on)
import Data.List (sortBy)
import Data.Double.Conversion.Text

getManyJobCells :: [[ JobResult ]] -> Handler Table
getManyJobCells iss = do
    --iss <- getManyJobResults ids
    let cells :: M.Map (JobID, (SolverID,Name),(ConfigID,Name)) 
                       (M.Map (BenchmarkID,Name) Cell)
        cells = M.fromListWith M.union $ do
          p <- concat iss
          return ( ( getJobID p
                   , ( toSolverID p
                     , toSolverName p 
                     )
                   , ( toConfigID p
                     , toConfigName p
                     )
                   )
               , M.singleton ( toBenchmarkID p
                             , toBenchmarkName p
                             ) 
                     $ cell_for_job_pair p
               )
        headers = M.keys cells
        benchmarks' = M.keys $ foldr M.union M.empty $ M.elems cells
    return $ Table 
           { header = Cell { contents = [whamlet| Benchmark |]
                           , url = "nothing"
                           , tag = "nothing"
                           , tdclass = ""
                           , mjr = Nothing
                           } : map cell_for_solver headers
           , rows = for benchmarks' $ \ bench -> 
               ( cell_for_bench bench : ) $ for headers $ \ h -> 
                   case M.lookup bench ( M.findWithDefault M.empty h cells ) of
                       Nothing -> empty_cell
                       Just c -> c
           }

for :: [a] -> (a -> b) -> [b]
for = flip map

empty_cell :: Cell
empty_cell = 
    Cell { contents = [whamlet| |]
         , tdclass = fromString "empty"
         , url = fromString "nothing"
         , tag = fromString "OTHER"
         , mjr = Nothing
         }

cell_for_bench :: ToMarkup a => (BenchmarkID, a) -> Cell
cell_for_bench (bid, bname) = Cell
  { contents = [whamlet|
<a href=@{ShowBenchmarkInfoR bid}>#{bname} 
|]
  , tdclass = fromString "bench"
  , url = fromString ""
  , tag = fromString "nothing"
  , mjr = Nothing
  }

cell_for_solver :: (ToMarkup a, ToMarkup b) => (JobID, (SolverID, a), (t, b)) -> Cell
cell_for_solver (j,(sid, sname),(_, cname)) = Cell 
  { contents = [whamlet|
<a href=@{ShowSolverInfoR sid}>#{sname}</a>/#{cname}
(<a href=@{ShowJobInfoR j}>#{show j}</a>)
|] 
  , tdclass = fromString "solver"
  , url = fromString ""
  , tag = fromString "nothing"
  , mjr = Nothing
  }

cell_for_job_pair :: JobResult -> Cell
cell_for_job_pair result = 
    Cell { mjr = Just result
         , contents = [whamlet|
            <a class="pair-link" href=@{ShowJobPairR (getPairID result)}>
                #{toFixed 1 $ toCpuTime result} /
                #{toFixed 1 $ toWallclockTime result}
             |]
         , tdclass = getClass result
         , url = fromString "nothing"
         , tag = getClass result
         }

display :: JobIds -> [Transform] -> [Transform] -> Table -> Widget
display jids previous ts tab  = do
  summary jids previous tab
  [whamlet| 
      <a href=@{FlexibleTableR (Query []) jids}>remove following #{show (length ts)} transformations
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

summary :: JobIds -> [Transform] -> Table -> Widget
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
                          <a href=@{FlexibleTableR these jids}>these
                          | <a href=@{FlexibleTableR others jids}>others
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
                   <a href=@{FlexibleTableR these jids}>these
                <td>
                   <a href=@{FlexibleTableR others jids}>others
    |]
    
apply :: Transform -> Table -> Table
apply t tab = case t of
    Filter_Rows p -> 
            tab { rows = filter ( \ row -> predicate p $ map tag row ) $ rows tab }

predicate :: Predicate -> [Text] -> Bool
predicate p tags = case p of
    And fs -> and $ zipWith matches fs tags
    Not q -> not $ predicate q tags

matches :: Cell_Filter -> Text -> Bool
matches f c = case f of
    Any -> True
    Equals s -> s == c
    Not_Equals s -> s /= c
