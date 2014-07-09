module Handler.ShowJobPair where

import Import
import StarExec.Types
import StarExec.JobData
import StarExec.Persist
import Data.ByteString
import qualified Data.Text as T

int2Text :: Int -> Text
int2Text = T.pack . show

getShowJobPairR :: Int -> Handler Html
getShowJobPairR _pairId = do
  (QueryResult qStatus mPair) <- queryJobPair _pairId
  mJobResult <- getPersistJobResult _pairId
  (mj, mb, ms) <- case mJobResult of
                    Nothing -> return (QueryResult Latest Nothing, QueryResult Latest Nothing, QueryResult Latest Nothing)
                    Just jr -> do
                      qmj <- queryJobInfo $ jobResultInfoJobId jr
                      qmb <- queryBenchmarkInfo $ jobResultInfoBenchmarkId jr
                      qms <- querySolverInfo $ jobResultInfoSolverId jr
                      return (qmj, qmb, qms)
  let mJobInfo = queryResult mj
      mBenchmarkInfo = queryResult mb
      mSolverInfo = queryResult ms
      hasHtmlProof = case mPair of
        Nothing -> False
        Just pair -> Nothing /= (jobPairInfoHtmlProof pair)
  defaultLayout $ do
    $(widgetFile "show_job_pair")
