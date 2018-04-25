module Handler.ShowBenchmarkInfo where

import Import
import Presenter.StarExec.JobData
import Presenter.Internal.Stringish
import Presenter.Utils.WidgetMetaRefresh
import Presenter.PersistHelper
import Presenter.History
import qualified Presenter.DOI as DOI
import Data.Maybe
import Control.Monad (guard)
import Data.List ( nub )

getBenchmarkType :: Benchmark -> Text
getBenchmarkType (StarExecBenchmark b) = benchmarkInfoType b
getBenchmarkType _ = notAvailable

getLastUpdate :: Benchmark -> Text
getLastUpdate (StarExecBenchmark b) = fromString $ show $ benchmarkInfoLastUpdate b
getLastUpdate _ = notAvailable

getFile :: Benchmark -> Text
getFile (LriBenchmark b) = lriBenchmarkInfoFile b
getFile _ = notAvailable

getRating :: Benchmark -> Text
getRating (LriBenchmark b) = fromString $ show $ lriBenchmarkInfoRating b
getRating _ = notAvailable

getSolved :: Benchmark -> Text
getSolved (LriBenchmark b) = fromString $ show $ lriBenchmarkInfoSolved b
getSolved _ = notAvailable

isConditional :: Benchmark -> Text
isConditional (LriBenchmark b) = fromBool $ lriBenchmarkInfoIsConditional b
isConditional _ = no

isContextSensitive :: Benchmark -> Text
isContextSensitive (LriBenchmark b) = fromBool $ lriBenchmarkInfoIsContextSensitive b
isContextSensitive _ = no

isInnermost :: Benchmark -> Text
isInnermost (LriBenchmark b) = fromBool $ lriBenchmarkInfoIsInnermost b
isInnermost _ = no

isOutermost :: Benchmark -> Text
isOutermost (LriBenchmark b) = fromBool $ lriBenchmarkInfoIsOutermost b
isOutermost _ = no

isRelative :: Benchmark -> Text
isRelative (LriBenchmark b) = fromBool $ lriBenchmarkInfoIsRelative b
isRelative _ = no

isTheory :: Benchmark -> Text
isTheory (LriBenchmark b) = fromBool $ lriBenchmarkInfoIsTheory b
isTheory _ = no

getBenchmarkUsageR :: BenchmarkID -> Handler Html
getBenchmarkUsageR bid = do
  resolver <- doiService <$> getYesod

  let eq_key = case DOI.fromBench resolver bid of
        Just doi -> EqDOI doi
        Nothing -> EqID bid
      query = Query [Filter_Benchmarks eq_key]

  let bids = nub $ bid : do
        doi <- maybeToList $ DOI.fromBench resolver bid
        DOI.toBenches resolver doi
  
  qjobs <- queryManyJobs $ map snd allCompetitionJobIDs
  let usage = nub $ do
        ((y,jid), q) <- zip allCompetitionJobIDs qjobs
        jr <- snd $ queryResult q
        guard $ DOI.fromBench resolver bid == DOI.fromBench resolver (toBenchmarkID jr)
        return (y, jid)
  defaultLayout $ do
    [whamlet|
<h1>Benchmark usage info

<p>has equivalent benchmarks (from different TPDB editions)

<p>
  $forall bid <- bids
    <a href=@{ShowBenchmarkInfoR bid}>#{show bid},

<p>appears in these competitions jobs
  (<a href=@{FlexibleTableR query (JobIds (map snd usage))}>
    see all
  )

<div class="container">
  <table class="table" style="width: auto;">
    <tbody>
      $forall (y, jid) <- usage
        <tr>
          <td>
            <a href=@{CompetitionR (CRefYear y)}>
              #{show y}
          <td>
            #{show jid}
          <td>  
            <a href=@{FlexibleTableR NoQuery (JobIds [jid])}>full job
          <td>
            <a href=@{FlexibleTableR query (JobIds [jid])}>selected benchmarks only

|]
  

getShowBenchmarkInfoR :: BenchmarkID -> Handler Html
getShowBenchmarkInfoR bid@(StarExecBenchmarkID _id) = do
  (QueryResult qStatus mBenchmarkInfo) <- queryBenchmarkInfo bid

  defaultLayout $ do
    case qStatus of
      Latest -> return ()
      Pending _ -> insertWidgetMetaRefresh
    [whamlet|
$maybe benchInfo <- mBenchmarkInfo
  <h1>Benchmark "#{toBenchmarkName benchInfo}"

  <div class="container">
    <table class="table" style="width: auto;">
      <tbody>
        <tr>
          <td>view original benchmark-info on star-exec:
          <td>
            <a href="https://www.starexec.org/starexec/secure/details/benchmark.jsp?id=#{show _id}">#{show _id}
        <tr>
          <td>Name:
          <td>#{toBenchmarkName benchInfo}
        <tr>
          <td>Type:
          <td>#{getBenchmarkType benchInfo}
        <tr>
          <td>Last Update from Star-Exec:
          <td>#{getLastUpdate benchInfo}
        <tr>
          <td>
            <a href=@{RenderBenchmarkR bid}>
              show benchmark source
        <tr>
          <td>
            <a href=@{BenchmarkUsageR bid}>
              usage in competitions
            
$nothing
  <h1>no benchmark with id #{show _id}
  (try re-loading this page after a while)
|]

getShowBenchmarkInfoR bid@(LriBenchmarkID _id) = do
  mBenchmarkInfo <- getPersistBenchmarkInfo bid
  defaultLayout $ do
    [whamlet|
$maybe benchInfo <- mBenchmarkInfo
  <h1>Benchmark-Infos of "#{toBenchmarkName benchInfo}"

  <div class="container">
    <table class="table" style="width: auto;">
      <tbody>
        <tr>
          <td>Name:
          <td>#{toBenchmarkName benchInfo}
        <tr>
          <td>File:
          <td>#{getFile benchInfo}
        <tr>
          <td>Rating:
          <td>#{getRating benchInfo}
        <tr>
          <td>Solved:
          <td>#{getSolved benchInfo}
        <tr>
          <td>isConditional:
          <td>#{isConditional benchInfo}
        <tr>
          <td>isContextSensitive:
          <td>#{isContextSensitive benchInfo}
        <tr>
          <td>isInnermost:
          <td>#{isInnermost benchInfo}
        <tr>
          <td>isOutermost:
          <td>#{isOutermost benchInfo}
        <tr>
          <td>isRelative:
          <td>#{isRelative benchInfo}
        <tr>
          <td>isTheory:
          <td>#{isTheory benchInfo}
$nothing
  <h1>no benchmark with the id #{show _id}
|]

