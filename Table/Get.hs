module Table.Get where

import Import

import Table.Data
import StarExec.JobData ( getManyJobResults )
import StarExec.Types


import qualified Data.Text as T

getManyJobCells ids = do
    iss <- getManyJobResults ids
    return $ Table 
           { header = [] -- FIXME
           , rows = map (map ( \ i -> Cell { contents = T.pack $ show $ persistJobResultInfoCpuTime i
                                           , url = T.pack "FIXME"
                                           , tag = T.pack $ show $ persistJobResultInfoResult i
                                           } ) ) iss
           }

