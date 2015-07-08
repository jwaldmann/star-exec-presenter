module Handler.DbTest where

import Import
import Presenter.PersistHelper


getDbTestR :: JobID -> Handler Html
getDbTestR jid = do
  jobResults <- getPersistJobResults jid
  let n = length jobResults
  defaultLayout [whamlet|
    #{show n}
    <ul>
    $forall i <- jobResults
      <li> #{show i}
    |]
