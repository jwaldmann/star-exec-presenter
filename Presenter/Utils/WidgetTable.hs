{-# language LambdaCase #-}

module Presenter.Utils.WidgetTable where

import Import
import Text.Blaze (ToMarkup)

import Presenter.Short
import Presenter.Processing
  ( getClass, BenchmarkKey, getBenchmark )
import qualified Presenter.DOI as DOI
import Presenter.Model.Additional.Table
import Presenter.Internal.Stringish

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad ( when, guard, mzero)
import Data.Maybe (catMaybes)

import Data.Function (on)
import Data.List (sortBy, inits, tails, minimumBy, maximumBy)
import Prelude (init)

import qualified Data.Graph.Inductive as G
import Data.Double.Conversion.Text

bminfo dois (key,name) = case key of
  Left bid -> (Left bid, name)
  Right doi -> (Right doi , maybe ( T.pack $ show doi ) id $ DOI.toName dois doi)

getManyJobCells :: [[ JobResult ]] -> Handler Table
getManyJobCells iss = do
    --iss <- getManyJobResults ids

    dois <- doiService <$> getYesod
  
    let cells :: M.Map (JobID, (SolverID,Name),(ConfigID,Name))
                       (M.Map (BenchmarkKey,Name) Cell)
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
               , M.singleton ( bminfo dois $ getBenchmark p )
                     $ cell_for_job_pair p
               )
        headers = M.keys cells
        benchmarks' = M.keys $ foldr M.union M.empty $ M.elems cells
    return $ Table
           { header = empty_cell { contents = [whamlet| Benchmark |]
                           } : map cell_for_solver headers
           , rows = for benchmarks' $ \ bench ->
               ( cell_for_bench bench : ) $ for headers $ \ h ->
                   case M.lookup bench ( M.findWithDefault M.empty h cells ) of
                       Nothing -> empty_cell
                       Just c -> c
           }

empty_cell :: Cell
empty_cell =
    Cell { contents = [whamlet| |]
         , tdclass = fromString "empty"
         , url = fromString "nothing"
         , tag = fromString "OTHER"
         , msolver = Nothing
         , mjr = Nothing
         , mjid = Nothing
         }

cell_for_job jid =
    empty_cell { contents = [whamlet|
         virtual best of <a href=@{ShowJobInfoR jid}>#{show jid}
        |] }

cell_for_bench
  :: (BenchmarkKey, Name)
  -> Cell
cell_for_bench (bkey, bname) = Cell
  { contents = [whamlet|
$case bkey
  $of Left bId
    <a href=@{ShowBenchmarkInfoR bId}>#{T.takeEnd 40 bname}
  $of Right doi
    <a href=@{ResolveR doi}>#{T.takeEnd 40 bname}
|]
  , tdclass = fromString "bench"
  , url = fromString ""
  , tag = fromString "nothing"
  , mjr = Nothing
  }

cell_for_solver :: (JobID, (SolverID, Text), (ConfigID, Text)) -> Cell
cell_for_solver (jid,(sid, sname),(cid, cname)) = Cell
  { contents = [whamlet|
<table>
  <tr>
    <td>
      <a href=@{ShowSolverInfoR sid}>#{sname}</a>
  <tr>
    <td>
      $case cid
        $of StarExecConfigID i
          <a href="https://www.starexec.org/starexec/secure/details/configuration.jsp?id=#{i}"> #{cname}</a>
        $of _
          #{cname}
  <tr>
    <td>
      Job <a href=@{ShowJobInfoR jid}>#{show jid}</a>
|]
  , tdclass = fromString "solver"
  , msolver = Just sname
  , url = fromString ""
  , tag = fromString "nothing"
  , mjr = Nothing
  , mjid = Just jid
  }

cell_for_job_pair :: JobResult -> Cell
cell_for_job_pair result =
    Cell { mjr = Just result
         , contents = [whamlet|
            <a class="pair-link" href=@{ShowJobPairR (getPairID result)}>
               #{short $ getSolverResult result }
               <span style="opacity:0.5">
                    #{toFixed 1 $ toCpuTime result} /
                    #{toFixed 1 $ toWallclockTime result}
             |]
         , tdclass = getClass result
         , msolver = Just $ toSolverName result
         , url = fromString "nothing"
         , tag = getClass result
         }

-- | old behaviour was: True
show_intermediate_transforms :: Bool
show_intermediate_transforms = False

display :: Scoring -> JobIds -> [Transform] -> [Transform] -> Table -> Widget
display sc jids previous ts tab  = do
  case ts of
    (t:later) -> do
        when show_intermediate_transforms $ do
          summary sc jids previous tab
          [whamlet|
            <a href=@{ShowManyJobResultsR sc (Query []) jids}>remove following #{show (length ts)} transformations
            <h3>apply transformation
               <pre>#{show t}
          |]
        display sc jids (previous ++ [t]) later $ apply (getIds jids) t tab

    [] -> do
        let q f = FlexibleTableR (Query $ previous ++ [f]) jids
        [whamlet|
            <h3>use virtual best solvers #
              (<a href=@{q VBestAll}>for all jobs</a> #
              | <a href=@{q VBestInit}>for all jobs except last</a>
              )
        |]
        summary sc jids previous tab
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

displayConcept :: JobIds -> Table -> Widget
displayConcept jids tab = do
  let rs = rows tab
  let indices = [1..length rs]
  [whamlet|
       <h3>Result table
       <table class="table">
        <thead>
          <tr>
              $forall h <- header tab
                  <th> ^{contents h}
        <tbody>
              $forall (row, i) <- zip rs indices
                  <tr>
                      $forall cell <- row
                        <td class="#{tdclass cell}"> ^{contents cell}
                  $if (mod i 20 == 0)
                       <tr>
                          $forall h <- header tab
                            <th> ^{contents h}
      |]

supertypes :: [a] -> [[Maybe a]]
supertypes xs = sequence $ map ( \ x -> [Just x, Nothing] ) xs

predecessors :: [Maybe a] -> [[Maybe a]]
predecessors xs = do
  (pre, Just x : post) <- splits xs
  return $ pre ++ Nothing : post

splits :: [a] -> [([a], [a])]
splits xs = zip (inits xs) (tails xs)

-- ignore nodes with exactly one successor
remove_units :: G.DynGraph gr => gr a () -> gr a ()
remove_units g =
  case G.gsel (\ c -> 1 == length (G.suc' c)) g of
    c : _ -> remove_units
       $ G.insEdges ( do p <- G.pre' c ; q <- G.suc' c ; return (p,q,()) )
       $ G.delNode (G.node' c) g
    _ -> g

summary :: Scoring -> JobIds -> [Transform] -> Table -> Widget
summary sc jids previous tab = do
    render <- getUrlRender
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
        row_type_stats = M.fromListWith (M.unionWith (+)) $ do
            row <- rows tab
	    let tags = map tag row
	    let keep t = t /= "nothing" && t /= "solver-maybe" && t /= "solver-other"
	        height = length $ filter keep tags
            return (height, M.singleton tags 1)
        row_type_table =
	   for (M.toAscList row_type_stats) $ \ (h, s) -> 
	    ( h
	    , for ( sortBy (flip compare `on` snd) $ M.toList s )
              	 $ \ (rt, n) ->
                (rt, n, Query (previous ++ [ Filter_Rows (And (map Equals rt)) ] )
                      , Query (previous ++ [ Filter_Rows (Not (And (map Equals rt))) ] )
                )
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
                          <a href=@{ShowManyJobResultsR sc these jids}>these
                          | <a href=@{ShowManyJobResultsR sc others jids}>others
                    $else
                        <td>
        <h3>row types
        <table class="table">
          <thead>
           <tr>
             $forall h <- header tab
                <th> ^{contents h}
          <tbody>
            $forall (h, tab) <- row_type_table
              <tr>
                <td>
                <td colspan="#{width}">
                  answered by #{h} solvers
              $forall (rt, n, these,others) <- tab
                <tr>
                  $forall t <- rt
                      <td class="#{t}"> #{t}
                  <td> #{show n}
                  <td>
                     <a href=@{ShowManyJobResultsR sc these jids}>these
                  <td>
                     <a href=@{ShowManyJobResultsR sc others jids}>others
    |]

data Pick = Best JobID [Int] | Copy Int deriving (Eq, Ord, Show)

apply :: [JobID] -> Transform -> Table -> Table
apply jids t tab = case t of
    Filter_Rows p ->
      tab { rows = filter ( \ row -> predicate p $ map tag row )
                 $ rows tab }
    VBestAll -> virtual_best jids t tab
    VBestInit -> virtual_best (init' jids) t tab

init' = reverse . drop 1 . reverse

virtual_best jids t tab =
        let transformed = S.fromList jids
            -- maps column index (in output table) to list of indices (in input table)
            collector = M.fromListWith (++) $ do
              (k, c @ Cell { mjid = Just jid }) <- zip [0..] $ header tab
              guard $ S.member jid transformed
              return (jid, [k])
            u : naffected = S.toList
                       $ S.difference
                         (S.fromList $ take (length $ header tab) [0..])
                       $ S.unions (map S.fromList $ M.elems collector )
            plan = Copy u : map (\(jid,ks) -> Best jid ks) (M.toList collector) ++ map Copy naffected
            app f cs = for plan $ \ case
              Copy k -> cs !! k
              Best t ks -> f t $ map (cs !!) ks
            snames = catMaybes $ map msolver $ header tab
        in  Table { header = app ( \ j _ -> cell_for_job j ) $ header tab 
                  , rows = map (app ( \ j cs -> best cs )) $ rows tab
                  }

-- | build cell for result of one benchmark by best virtual solver
best :: [Cell] -> Cell
best [] = empty_cell
best cs =
  ( \ c ->  c { contents = do
                  case msolver c of
                    Just s | interesting c -> [whamlet|#{s}|]
                    _ -> return ()
                  contents c 
                }
  ) $ foldr1 ( \ x y -> if interesting x
                      then if interesting y then merge x y
                           else x else y ) cs

-- FIXME: need to do something better here,
-- compare bounds, add times, etc.
merge b c =
  let tags = catMaybes $ map msolver [b,c]
  in  b { msolver = case tags of
            [] -> Nothing
            ts -> Just $ T.intercalate "," ts
        }

interesting c =
  case mjr c of
    Nothing -> False
    Just jr -> case getSolverResult jr of
      YES -> True ; NO -> True ; BOUNDS {} -> True
      _ -> False
      

normalize b = case (lower b, upper b) of
  (Infinite , _ ) -> NO
  (Finite, Finite) -> YES
  _ -> BOUNDS b

getlower r = case getSolverResult r of
  NO -> return Infinite
  BOUNDS ( Bounds { lower = l } ) -> return l
  _ -> mzero

getupper r = case getSolverResult r of
  YES -> return Finite
  BOUNDS ( Bounds { upper = u } ) -> return u
  _ -> mzero
    

predicate :: Predicate -> [Text] -> Bool
predicate p tags = case p of
    And fs -> and $ zipWith matches fs tags
    Not q -> not $ predicate q tags

matches :: Cell_Filter -> Text -> Bool
matches f c = case f of
    Any -> True
    Equals s -> s == c
    Not_Equals s -> s /= c
