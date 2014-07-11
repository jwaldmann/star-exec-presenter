module Handler.Flexible_Table where

import Import

import StarExec.Types 
import StarExec.JobData ( queryManyJobResults )
import Table.Data
import Table.Query
import Table.Get
import Utils.WidgetMetaRefresh

import Text.Lucius (luciusFile)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Function (on)
import Data.List (sortBy)

getFlexible_TableR :: Query -> JobIds -> Handler Html
getFlexible_TableR q @ (Query ts) jids @ (JobIds ids) = do
  qJobs <- queryManyJobResults ids
  tab <- Table.Get.getManyJobCells $ map queryResult qJobs
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
