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
postConceptsR :: JobID -> Handler Html
postConceptsR jid = do
  --QueryResult qStatus _ <- queryJob jid
  context <- jobResultsContext jid
  -- perhaps reduction of some attributes
  -- postedText <- runInputPost $ ireq textField "content"
  --postedText <- runInputPost $ ireq checkBoxField "content"


  let concepts' = concepts context
  defaultLayout $ do
    -- fetch job from starexec if not present in database
    -- when (qStatus /= Latest)
    --  insertWidgetMetaRefresh
    setTitle "concepts"
    $(widgetFile "concepts")


getConceptsR :: JobID -> Handler Html
getConceptsR jid =  do
  context <- jobResultsContext jid
  let attrs = attributes context
  defaultLayout $(widgetFile "concepts_attributes")
