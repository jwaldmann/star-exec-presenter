module Handler.Concepts where

import Import
import Data.List (elemIndex)
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad (when)
import Presenter.PersistHelper
import Presenter.Model.Entities()
import Presenter.StarExec.JobData (queryJob)
import Presenter.Utils.WidgetMetaRefresh (insertWidgetMetaRefresh)
import FCA.Utils
import FCA.StarExec

-- route to show concepts of given JobID
getConceptsR :: JobID -> Handler Html
getConceptsR jid = do
  QueryResult qStatus _ <- queryJob jid
  jobResults <- getPersistJobResults jid
  let contextData = collectData $ getStarExecResults jobResults
  let context = contextFromList contextData
  let concepts' = concepts context
  defaultLayout $ do
    -- fetch job from starexec if not present in database
    when (qStatus /= Latest)
     insertWidgetMetaRefresh
    setTitle "concepts"
    $(widgetFile "concepts")
