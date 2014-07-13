module Table.Get where

import Import

import Table.Data
import StarExec.JobData ( getClass )
import StarExec.Types


import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Double.Conversion.Text

getManyJobCells iss = do
    --iss <- getManyJobResults ids
    let cells :: M.Map (Text,Text) (M.Map Text Cell)
        cells = M.fromListWith M.union $ do
          p <- concat iss
          return ( (jobResultInfoSolver p, jobResultInfoConfiguration p)
               , M.singleton (jobResultInfoBenchmark p) $ cell_for_job_pair p
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

cell_for_bench b = Cell { contents = [whamlet| #{b} |]
                        , tdclass = T.pack "bench"
                        , url = T.pack $ "URL for bench " ++ T.unpack b
                        , tag = T.pack "nothing"
                        }

cell_for_solver (s,c) = Cell { contents = [whamlet| #{s}/#{c} |] 
                        , tdclass = T.pack "solver"
                        , url = T.intercalate (T.pack " ")
                          [ T.pack "URL for solver/config" , s, c ]
                        , tag = T.pack "nothing"
                        }

cell_for_job_pair result = 
    Cell { mjri = Just result
         , contents = [whamlet|
            <a class="pair-link" href=@{ShowJobPairR (jobResultInfoPairId result)}>
                #{toFixed 1 $ jobResultInfoCpuTime result} /
                #{toFixed 1 $ jobResultInfoWallclockTime result}
             |]
         , tdclass = getClass result
         , url = T.pack $ "nothing"
         , tag = getClass result
         }
