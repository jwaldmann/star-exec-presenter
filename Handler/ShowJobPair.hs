module Handler.ShowJobPair where

import Import
import StarExec.Types
import StarExec.JobData
import StarExec.Persist

getShowJobPairR :: Int -> Handler Html
getShowJobPairR _pairId = do
  (QueryResult qStatus mPair) <- queryJobPair _pairId
  mJobResult <- getPersistJobResult _pairId
  (mj, mb, ms) <- case mJobResult of
                    Nothing -> return (Nothing, Nothing, Nothing)
                    Just jr -> do
                      qmj <- queryJobInfo $ jobResultInfoJobId jr
                      qmb <- queryBenchmarkInfo $ jobResultInfoBenchmarkId jr
                      qms <- querySolverInfo $ jobResultInfoSolverId jr
                      return (queryResult qmj, queryResult qmb, queryResult qms)
  let mJobInfo = mj
      mBenchmarkInfo = mb
      mSolverInfo = ms
  defaultLayout $ do
    $(widgetFile "show_job_pair")
