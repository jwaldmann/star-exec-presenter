module Table.Get where

import Import

import Table.Data
import StarExec.Processing ( getClass )
import StarExec.Types


import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Double.Conversion.Text

getManyJobCells :: [[ JobResultInfo ]] -> Handler Table
getManyJobCells iss = do
    --iss <- getManyJobResults ids
    let cells :: M.Map (Int, (Int,Text),(Int,Text)) 
                       (M.Map (Int,Text) Cell)
        cells = M.fromListWith M.union $ do
          p <- concat iss
          return ( ( jobResultInfoJobId p
                   , ( jobResultInfoSolverId p
                     , jobResultInfoSolver p 
                     )
                   , ( jobResultInfoConfigurationId p
                     , jobResultInfoConfiguration p
                     )
                   )
               , M.singleton ( jobResultInfoBenchmarkId p
                             , jobResultInfoBenchmark p
                             ) 
                     $ cell_for_job_pair p
               )
        headers = M.keys cells
        benchmarks = M.keys $ foldr M.union M.empty $ M.elems cells
    return $ Table 
           { header = Cell { contents = [whamlet| Benchmark |]
                           , url = T.pack "nothing"
                           , tag = T.pack "nothing"
                           } : map cell_for_solver headers
           , rows = for benchmarks $ \ bench -> 
               ( cell_for_bench bench : ) $ for headers $ \ h -> 
                   case M.lookup bench ( M.findWithDefault M.empty h cells ) of
                       Nothing -> empty_cell
                       Just c -> c
           }

for = flip map

empty_cell = 
    Cell { contents = [whamlet| |], tdclass = T.pack "empty"
         , url = T.pack "nothing"
         , tag = T.pack "OTHER" }

cell_for_bench (bid, bname) = Cell { contents = [whamlet|
<a href=@{ShowBenchmarkInfoR bid}>#{bname} 
|]
                        , tdclass = T.pack "bench"
                        , url = ""
                        , tag = T.pack "nothing"
                        }

cell_for_solver (j,(sid, sname),(cid, cname)) = Cell 
    { contents = [whamlet|
<table>
  <tr>
    <td>
      <a href=@{ShowSolverInfoR sid}>#{sname}</a>
  <tr>
    <td>   
      <a href="https://www.starexec.org/starexec/secure/details/configuration.jsp?id=#{cid}"> #{cname}</a>
  <tr>
    <td>
      Job <a href=@{ShowJobInfoR j}>#{j}</a>
|] 
    , tdclass = T.pack "solver"
    , url = ""
    , tag = T.pack "nothing"
    }

cell_for_job_pair result = 
    Cell { mjri = Just result
         , contents = [whamlet|
            <a class="pair-link" href=@{ShowJobPairR (jobResultInfoPairId result)}>
                #{toFixed 1 $ jobResultInfoCpuTime result} /
                #{toFixed 1 $ jobResultInfoWallclockTime result}
                $maybe os <- jobResultInfoOutputSize result
                  / #{os}
             |]
         , tdclass = getClass result
         , url = T.pack $ "nothing"
         , tag = getClass result
         }
