module Handler.ShowJobPair where

import Import
import StarExec.Types
import Presenter.RouteTypes
import Presenter.Models
import StarExec.JobData
import StarExec.Persist
import qualified Data.Text as T
import Utils.WidgetMetaRefresh

int2Text :: Int -> Text
int2Text = T.pack . show

getShowJobPairR :: Int -> Handler Html
getShowJobPairR _pairId = do
  (QueryResult qStatus mPair) <- queryJobPair _pairId
  mJobResult <- getPersistJobResult _pairId
  (mj, mb, ms) <- case mJobResult of
                    Nothing -> return (QueryResult Latest (Nothing, []), QueryResult Latest Nothing, QueryResult Latest Nothing)
                    Just jr -> do
                      qmj <- queryJob $ jobResultInfoJobId jr
                      qmb <- queryBenchmarkInfo $ jobResultInfoBenchmarkId jr
                      qms <- querySolverInfo $ jobResultInfoSolverId jr
                      return (qmj, qmb, qms)
  let mJobInfo = fst $ queryResult mj
      mBenchmarkInfo = queryResult mb
      mSolverInfo = queryResult ms
      hasHtmlProof = case mPair of
        Nothing -> False
        Just pair -> Nothing /= (jobPairInfoHtmlProof pair)
  defaultLayout $ do
    case qStatus of
      Latest -> return ()
      Pending _ -> insertWidgetMetaRefresh
    $(widgetFile "show_job_pair")
