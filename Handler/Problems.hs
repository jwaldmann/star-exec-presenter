module Handler.Problems
  ( getProblemsR
  ) where

import Import
import Presenter.StarExec.JobData
import Presenter.Model (Function(..))
import Presenter.Processing (getupperbound, getlowerbound,getClass)
import Handler.ShowManyJobResults (shorten)
import Presenter.Utils.WidgetMetaRefresh
import Text.Lucius (luciusFile)
import Presenter.Short

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad ( guard, when )
import Data.Maybe (catMaybes)

-- | this returns true iff  Omega(lower) `intersect` Oh(upper) == empty.
incompatible :: Function -> Function -> Bool
incompatible lower upper = case (lower,upper) of
  (Infinite, _ ) ->  upper /= Infinite
  (Poly (Just a), Poly (Just b)) -> a > b
  _ -> False

-- | display all job pairs with strange results (not YES, NO, MAYBE, WORST_CASE),
-- as these are the most interesting to look at before competition.
-- https://github.com/stefanvonderkrone/star-exec-presenter/issues/86
getProblemsR :: JobIds -> Handler Html
getProblemsR jids @ (JobIds ids) = do
  qJobs <- queryManyJobs ids
  let jobInfos = catMaybes $ map (fst . queryResult) qJobs
      jobResults = concat $ map (snd . queryResult) qJobs
      bybench = M.fromListWith (++) $ do
        jr <- jobResults
        return ( (toBenchmarkID jr,toBenchmarkName jr) , [ jr ] )
      yesno = do
        (bench, jrs) <- M.toList bybench
        me <- jrs ; you <- jrs
        let lo = getlowerbound me ; hi = getupperbound you
        guard $ incompatible lo hi
        return ( bench ,(me,lo),(you,hi))
      others = do
        jr <- jobResults
        let good = case getSolverResult jr of
              YES -> True;NO -> True;MAYBE -> True ; BOUNDS {} -> True ; CERTIFIED -> True
              OTHER {} -> False; ERROR -> False
        guard $ not good
        return jr
  defaultLayout $ do
    toWidget $(luciusFile "templates/solver_result.lucius")
    when ( any (\q -> case queryStatus q of Latest -> False ; _ -> True) qJobs ) $ do
      insertWidgetMetaRefresh
    [whamlet|
       <h1>Consistency Check for Jobs
       $forall j <- jobInfos
           <a href=@{ShowJobInfoR $ toJobID j}>#{toJobName j}</a>,
       <h2>Incompatible lower and upper bounds
       <table class="table">
         <thead>
           <tr>
             <th>Benchmark
             <th>Job Pair A
             <th>lower bound
             <th>Job Pair B
             <th>upper bound
         <tbody>
           $forall ((bId,bName),(me,lo),(you,hi)) <- yesno
             <tr>
               <td>
                 <a href=@{ShowBenchmarkInfoR bId}>
                   #{shorten bName}
               <td>
                 <a href=@{ShowSolverInfoR (toSolverID me)}>
                   #{toSolverName me}
               <td class="#{getClass me}"> 
                 <a class="pair-link" href=@{ShowJobPairR (getPairID me)}>
                   #{short $ getSolverResult me} 
               <td>
                 <a href=@{ShowSolverInfoR (toSolverID you)}>
                   #{toSolverName you}
               <td  class="#{getClass you}">
                 <a class="pair-link" href=@{ShowJobPairR (getPairID you)}>
                   #{short $ getSolverResult you}
       <h2>Certification Problems
       in fact OTHER or ERROR from postprocessor (instead of YES NO MAYBE BOUNDS CERTIFIED)
       <table class="table">
         <thead>
           <tr>
             <th>Benchmark
             <th>Job Pair
             <th>Result
         <tbody>
           $forall me <- others
             <tr>
               <td>
                 <a href=@{ShowBenchmarkInfoR (toBenchmarkID me)}>
                   #{shorten (toBenchmarkName me)}
               <td>
                 <a href=@{ShowSolverInfoR (toSolverID me)}>
                   #{toSolverName me}
               <td class="#{getClass me}"> 
                 <a class="pair-link" href=@{ShowJobPairR (getPairID me)}>
                   #{short $ getSolverResult me} 
    |]


















