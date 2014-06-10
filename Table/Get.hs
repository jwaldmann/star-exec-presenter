module Table.Get where

import Import

import Table.Data
import StarExec.JobData ( getManyJobResults )
import StarExec.Types
import StarExec.Persist ( fromPersistJobResultInfo )


import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S

getManyJobCells ids = do
    iss <- getManyJobResults ids
    let cells :: M.Map (Text,Text) (M.Map Text Cell)
        cells = M.fromListWith M.union $ do
          p <- concat iss
          let i = fromPersistJobResultInfo p
          return ( (jriSolver i, jriConfiguration i)
               , M.singleton (jriBenchmark i) $ cell_for_job_pair i
               )
        headers = M.keys cells
        benchmarks = M.keys $ foldr M.union M.empty $ M.elems cells
    return $ Table 
           { header = Cell { contents = T.pack "Benchmark", url = T.pack "nothing"
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
    Cell { contents = T.pack "empty", url = T.pack "nothing", tag = T.pack "OTHER" }

cell_for_bench b = Cell { contents = b , url = T.pack $ "URL for bench " ++ T.unpack b
                        , tag = T.pack "nothing"
                        }

cell_for_solver (s,c) = Cell { contents = T.append s c
                        , url = T.intercalate (T.pack " ")
                          [ T.pack "URL for solver/config" , s, c ]
                        , tag = T.pack "nothing"
                        }
cell_for_job_pair i = 
    Cell { contents = T.pack $ show ( jriCpuTime i, jriWallclockTime i )
                          , url = T.pack $ "URL for jriPairId " ++ show (jriPairId i )
                          , tag = T.pack $ show $ jriResult i
                          }
