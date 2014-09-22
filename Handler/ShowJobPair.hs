module Handler.ShowJobPair where

import Import
import Presenter.StarExec.JobData
import Presenter.Internal.Stringish
import Presenter.Utils.WidgetMetaRefresh
import Presenter.PersistHelper

getShowJobPairR :: JobPairID -> Handler Html
getShowJobPairR pid@(StarExecPairID _id) = do
  (QueryResult qStatus mPair) <- queryJobPair pid
  mJobResult <- getPersistJobResult pid
  (mj, mb, ms) <- case mJobResult of
                    Just (StarExecResult jr) -> do
                      qmj <- queryJob $ StarExecJobID $ jobResultInfoJobId jr
                      qmb <- queryBenchmarkInfo $ StarExecBenchmarkID $ jobResultInfoBenchmarkId jr
                      qms <- querySolverInfo $ StarExecSolverID $ jobResultInfoSolverId jr
                      return (qmj, qmb, qms)
                    _ -> return (QueryResult Latest (Nothing, []), QueryResult Latest Nothing, QueryResult Latest Nothing)
  let mJobInfo = fst $ queryResult mj
      mBenchmarkInfo = queryResult mb
      mSolverInfo = queryResult ms
      hasHtmlProof = case mPair of
        Nothing -> False
        Just (StarExecPair pair) -> Nothing /= (jobPairInfoHtmlProof pair)
  defaultLayout $ do
    case qStatus of
      Latest -> return ()
      Pending _ -> insertWidgetMetaRefresh
    $(widgetFile "se_show_job_pair")
getShowJobPairR _ = error "Not yet implemented: getShowJobPairR"
