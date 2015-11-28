module Handler.Problems
  ( getProblemsR
  ) where

import Import
import Presenter.StarExec.JobData
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
      problematic = S.fromList $ do
         StarExecResult p <- others ++ do (bench,(me,lo),(you,hi)) <- yesno ; [ me,you]
         return $ StarExecJobID $ jobResultInfoJobId p
  let solver_result_cell me = [whamlet|
               <td>
                 <a href=@{ShowSolverInfoR (toSolverID me)}>
                   #{toSolverName me}
               <td>
                 <a href=@{ShowConfigInfoR (toConfigID me)}>
                   #{toConfigName me}
               <td class="#{getClass me}">
                 <a class="pair-link" href=@{ShowJobPairR (getPairID me)}>
                   #{short $ getSolverResult me}
           |]
      benchmark_cell me = [whamlet|
               <td>
                 <a href=@{ShowBenchmarkInfoR (toBenchmarkID me)}>
                   #{shorten (toBenchmarkName me)}
           |]
  defaultLayout $ do
    toWidget $(luciusFile "templates/solver_result.lucius")
    when ( any (\q -> case queryStatus q of Latest -> False ; _ -> True) qJobs ) $ do
      insertWidgetMetaRefresh
    [whamlet|
       <h1>Consistency Check for Jobs
       $forall j <- jobInfos
           <a href=@{ShowJobInfoR $ toJobID j}>#{toJobName j}</a>,
       <p>the subset of these jobs that actually have problems:
          <a href=@{ProblemsR $ JobIds $ S.toList problematic}>#{show problematic}
       <h2>Incompatible Lower and Upper Bounds
       <table class="table">
         <thead>
           <tr>
             <th>Benchmark
             <th>Solver A
             <th>Config A
             <th>Lower Bound A
             <th>Solver B
             <th>Config B
             <th>Upper Bound B
         <tbody>
           $forall ((bId,bName),(me,lo),(you,hi)) <- yesno
             <tr>
               ^{benchmark_cell me}
               ^{solver_result_cell me}
               ^{solver_result_cell you}
       <h2>Strange Results
       <p>
         Normal results are
         <span class="solver-yes">YES,
         <span class="solver-no">NO,
         <span class="solver-maybe">MAYBE,
         <span class="solver-certified">CERTIFIED,
         <span class="solver-bounds">BOUNDS.

       <p>
         Strange results are
         <ul>
           <li>
             <span class="solver-other">OTHER
             (indicating an error in the solver, e.g., a reject from the certifier)
           <li>
             <span class="solver-error">ERROR
             (indicating an error in the postprocessor).
       <table class="table">
         <thead>
           <tr>
             <th>Benchmark
             <th>Solver
             <th>Config
             <th>Result
         <tbody>
           $forall me <- others
             <tr>
               ^{benchmark_cell me}
               ^{solver_result_cell me}
    |]
