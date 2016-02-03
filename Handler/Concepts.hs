module Handler.Concepts where

import Import
import Data.List (elemIndex)
import Data.Maybe
import Control.Monad (when)
import Presenter.StarExec.JobData (queryJob)
import Presenter.Utils.WidgetMetaRefresh (insertWidgetMetaRefresh)
import FCA.Utils
import FCA.StarExec

-- route to show concepts of given JobID
getConceptsR :: JobID -> Handler Html
getConceptsR jid = do
  QueryResult qStatus _ <- queryJob jid
  context <- jobResultsContext jid
  let concepts' = concepts context
  defaultLayout $ do
    -- fetch job from starexec if not present in database
    when (qStatus /= Latest)
     insertWidgetMetaRefresh
    setTitle "concepts"
    $(widgetFile "concepts")
