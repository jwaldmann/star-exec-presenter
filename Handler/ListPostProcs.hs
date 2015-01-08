module Handler.ListPostProcs where

import Import
import Presenter.PersistHelper

getAll :: Handler [PostProcInfo]
getAll = runDB $ do
  getEntityList' ([] :: [Filter PostProcInfo]) []

getListPostProcsR :: Handler Html
getListPostProcsR = do
  postProcs <- getAll
  defaultLayout $ do
    $(widgetFile "list_post_procs")
