module Handler.ListPostProcs where

import Import
import Presenter.PersistHelper

getAll :: Handler [PostProcInfo]
getAll = runDB_readlocked $ do
  getEntityList' ([] :: [Filter PostProcInfo]) []

getListPostProcsR :: Handler Html
getListPostProcsR = do
  postProcs <- getAll
  defaultLayout $ do
    $(widgetFile "list_post_procs")
