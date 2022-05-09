{-# language LambdaCase, RankNTypes #-}
{-# options_ghc -Werror=incomplete-patterns #-}

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
import Data.List (sort, sortOn, sortBy, inits, tails, minimumBy, maximumBy, transpose)
import Prelude (init)
import qualified Data.List as L

import qualified Data.Graph.Inductive as G
import Data.Double.Conversion.Text

import Data.Csv (toRecord, encode, ToField(..))

bminfo dois (key,name) = case key of
  Left bid -> (Left bid, name)
  Right doi -> (Right doi , maybe ( T.pack $ show doi ) id $ DOI.toName dois doi)

data CombineJobs = Union | Intersection

getManyJobCells :: [[ JobResult ]] -> Handler Table
getManyJobCells = getManyJobCellsCombined Union

getManyJobCellsCombined combi iss = do
    --iss <- getManyJobResults ids

    dois <- doiService <$> getYesod

    let cells_per_group = map cpg iss
        cpg is = M.fromListWith M.union $ do
          p <- is
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
        common_benchmarks = if null cells_per_group then S.empty else
          foldl1 S.intersection $ do
            cs <- cells_per_group
            return $ S.fromList $ do
              m <- M.elems cs
              M.keys m
              
        cells :: M.Map (JobID, (SolverID,Name),(ConfigID,Name))
                       (M.Map (BenchmarkKey,Name) Cell)
        cells = M.unionsWith M.union cells_per_group

        headers = M.keys cells
        benchmarks' = case combi of
          Union -> M.keys $ foldr M.union M.empty $ M.elems cells
          Intersection -> S.toList common_benchmarks
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
         , tag = fromString "nothing"
         , nums = M.empty
         , msolver = Nothing
         , mbench = Nothing
         , mbenchkey = Nothing
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
cell_for_bench (bkey, bname) = empty_cell
  { contents = [whamlet|
$case bkey
  $of Left bId
    <a href=@{ShowBenchmarkInfoR bId}>#{T.takeEnd 40 bname}
  $of Right doi
    <a href=@{ResolveR doi}>#{T.takeEnd 40 bname}
|]
  , tdclass = fromString "bench"
  , url = fromString ""
  , mbench = Just bname
  , mbenchkey = Just bkey
  }

cell_for_solver :: (JobID, (SolverID, Text), (ConfigID, Text)) -> Cell
cell_for_solver (jid,(sid, sname),(cid, cname)) = empty_cell
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
      <a href=@{ShowJobInfoR jid}>#{show jid}</a>
|]
  , tdclass = fromString "solver"
  , msolver = Just sname
  , url = fromString ""
  , mjid = Just jid
  }

cell_for_job_pair :: JobResult -> Cell
cell_for_job_pair result = empty_cell
         { mjr = Just result
         , contents = [whamlet|
            <a class="pair-link" href=@{ShowJobPairR (getPairID result)}>
               #{short $ getSolverResult result }
               <span style="opacity:0.5">
                    #{toFixed 1 $ toCpuTime result} /
                    #{toFixed 1 $ toWallclockTime result}
                    $maybe os <- toOutputSize result
                      &nbsp;(#{os})
             |]
         , tdclass = getClass result
         , msolver = Just $ toSolverName result
         , tag = getClass result
         , nums = M.fromList $ do
             (t, Just d) <- [ (CPU, Just $ toCpuTime result)
                   , (Wall, Just $ toWallclockTime result)
                   , (Size, fromIntegral <$> toOutputSize result)
                   ]
             return (t, d)
         }

-- | old behaviour was: True
show_intermediate_transforms :: Bool
show_intermediate_transforms = False

displayCSV verbose sc NoQuery jids tab0 =
  displayCSV verbose sc (Query []) jids tab0

displayCSV verbose sc (Query ts) jids tab0 = do
  let jobname = T.pack $ concat $ L.intersperse "-" $ do
        id <- getIds jids
        return $ case id of
          StarExecJobID i -> show i
          _ -> "X"
      tab = foldl (flip $ apply $ getIds jids) tab0 ts
      compact :: forall a . [a] -> [a]
      compact = if verbose then id else take 1
  let head = 
        [ do c <- header tab
             map toField $ case msolver c of
               Nothing -> [ "benchmark" ]
               Just s ->  compact [ s, s, s, s ]
        , do c <- header tab
             case msolver c of
               Nothing -> [ " " ]
               Just s ->  compact [ "result", "cpu", "wall", "size" ] 
        ]
      show_tag t = case T.stripPrefix "solver-" t of
        Just s -> s
        _ -> t
      body = do
        b : cs <- rows tab
        return $ toField (maybe "" id $ mbench b) : do
          c <- cs
          compact $ toField (show_tag $ tag c) : do
            k <- [ CPU, Wall, Size ]
            return $ case M.lookup k $ nums c of
              Nothing -> " "
              Just v -> toField v
  let csv = encode $ map toRecord $ head ++ body
  addHeader "Content-Disposition"
    ( "attachment;filename=results-" <> jobname <> ".csv" )
  return $ TypedContent "text/csv" $ toContent csv

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
            $if null previous && null ts
              <h3>
                <a href=@{q Common}>
                  restrict to benchmarks common to all jobs
        |]
        summary sc jids previous tab
    -- no more transformers, display actual data
        let rs = rows tab
        [whamlet|
           <h3>data
           <p>
             also available in CSV format:
             <a href=@{ShowManyJobResultsCSVR False (Query previous) jids}>
               terse (just results)
             ,
             <a href=@{ShowManyJobResultsCSVR True (Query previous) jids}>
               verbose (results, times, size)
           <p>
             <table class="table">
              <thead>
                <tr>
                    $forall h <- header tab
                        <th>^{contents h}
              <tbody>
                    $forall (i,row) <- zip (enumFrom 1) rs
                        <tr>
                          $forall cell <- row
                            <td class="#{tdclass cell}"> ^{contents cell}
                        $if (0 == mod i 20)
                          <tr>
                            $forall h <- header tab
                              <th>^{contents h}
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
        column_nums_table :: [ M.Map Numtag (M.Map Level Double) ]
        column_nums_table = for (cols tab) $ \ col ->
          M.fromList $ do
            nt <- [ CPU , Wall, Size ]
            let values = Data.List.sort $ catMaybes $ do
                  c <- col
                  return $ M.lookup nt $ nums c
                n = length values
                avg = if null values then 0 else sum values / fromIntegral n
                m = M.fromList $ (Sum, sum values) :
                           [ (Min, values !! 0)
                           , (Bot, values !! div n cut)
                           , (Med, values !! div n 2)
                           , (Avg, avg)
                           , (Top, values !! div ((cut-1)*n) cut)
                           , (Max, values !! (n-1) )
                           ]
            guard $ not $ null values
            return (nt, m)
        sorter direction i nt = Query $ previous ++ [ Sort direction i nt ]
        comparer i nt ordering bound =
          Query $ previous ++ [ Filter_Rows (Compare i nt ordering bound) ]
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
        <h3>statistics
        <table class="table">
          <thead>
           <tr>
             $forall h <- header tab
                <th> ^{contents h}
          <tbody>
             <tr>
               <td>
                 <table class="table">
                   <thead>
                     <tr>
                       <th>
                         sort (inc)
                   <tbody>
                     $forall l <- levels
                       <tr>
                         <td>
                           #{explain l}
                   <thead>
                     <tr>
                       <th>
                         sort (dec)
               $forall (i, c) <- drop 1 (zip (enumFrom 0) column_nums_table)
                 <td>
                   <table class="table">
                     <thead>
                       <tr>
                         $forall (k,v) <- M.toList c
                           <th>
                             <a href=@{ShowManyJobResultsR sc (sorter Up i (Just k)) jids}>
                               #{show k} (#{unit k})
                     <tbody>
                       $forall l <- levels
                         <tr>
                           $forall (k,v) <- M.toList c
                             <td>
                               $maybe d <- M.lookup l v
                                 <a href=@{ShowManyJobResultsR sc (comparer i k LT d) jids}>&lt;
                                 #{shorten d}
                                 <a href=@{ShowManyJobResultsR sc (comparer i k GT d) jids}>&gt;

                     <thead>
                       <tr>
                         $forall (k,v) <- M.toList c
                           <th>
                             <a href=@{ShowManyJobResultsR sc (sorter Down i (Just k)) jids}>
                               #{show k} (#{unit k})
    |]

cut :: Int
cut = 10

explain :: Level -> T.Text
explain l = case l of
  Sum -> "sum"
  Max -> "maximum"
  Top -> "top "    <> T.pack (show cut) <> "%"
  Avg -> "average"
  Med -> "median"
  Bot -> "bottom " <> T.pack (show cut) <> "%"
  Min -> "minimum"

-- | show with 3 significant digits,
-- and unit prefix (milli to Tera)
shorten :: Double -> String
shorten 0 = "0"
shorten x | x < 0 = "-" <> shorten x
shorten x =
  let (y, f) = 
        if      x < 1e0 then (x *  1e3, "m")
        else if x < 1e3 then (x       , "" )
        else if x < 1e6 then (x /  1e3, "k")
        else if x < 1e9 then (x /  1e6, "M")
        else if x < 1e12 then (x /  1e9, "G")
        else                 (x / 1e12, "T")
      e = truncate $ logBase 10 y
      off = 2 - e
      p = 10^^off
      z = round $ y * p
      (pre,post) = splitAtEnd off $ show z
  in     (if null pre then "0" else pre)
      <> (if null post then "" else "." <> post)
      <> f

splitAtEnd k xs =
  let (ys,zs) = splitAt k $ reverse xs
  in  (reverse zs, reverse ys)
      
data Pick = Best JobID [Int] | Copy Int deriving (Eq, Ord, Show)

apply :: [JobID] -> Transform -> Table -> Table
apply jids t tab = case t of
    Filter_Rows p ->
      tab { rows = filter ( predicate p ) $ rows tab }
    VBestAll -> virtual_best jids t tab
    VBestInit -> virtual_best (init' jids) t tab
    Sort dir col (Just sub) ->
      let val :: Row -> Either Double ()
          val row = case M.lookup sub $ nums $ row !! col of
            Nothing -> Right () -- late in the order
            Just d -> Left $ ( case dir of Up -> id ; Down -> negate ) d
      in  tab { rows = sortOn val $ rows tab }
    Common -> tab -- because we filtered this earlier already
    Filter_Benchmarks p ->
      tab { rows = filter ( benchmark_predicate p ) $ rows tab }
    _ -> tab -- well...  

init' = reverse . drop 1 . reverse

virtual_best jids t tab =
        let transformed = S.fromList jids
            -- maps column index (in output table) to list of indices (in input table)
            collector = M.fromListWith (++) $ do
              (k, c@Cell { mjid = Just jid }) <- zip [0..] $ header tab
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
    

predicate :: Predicate -> Row -> Bool
predicate p row = case p of
    And fs -> and $ zipWith matches fs $ map tag row
    Not q -> not $ predicate q row
    Compare i nt ordering bound ->
      case M.lookup nt $ nums $ row !! i of
        Nothing -> False
        Just v -> (case ordering of LT -> (<=) ; GT -> (>=); EQ -> (==) ) v bound

benchmark_predicate p row = case p of
  EqDOI doi -> or $ do
    leader <- take 1 row
    return $ Just (Right doi) == mbenchkey leader
  EqID id -> or $ do
    leader <- take 1 row
    return $ Just (Left id) == mbenchkey leader
  NameMatches pat -> or $ do
    leader <- take 1 row
    return $ case mbench leader of
      Nothing -> False
      Just bench -> T.isInfixOf pat bench
  NotNameMatches pat ->
    not $ benchmark_predicate (NameMatches pat) row

matches :: Cell_Filter -> Text -> Bool
matches f c = case f of
    Any -> True
    Equals s -> s == c
    Not_Equals s -> s /= c
