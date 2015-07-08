module Handler.DbTest where

import Import

getDbTestR :: Handler Html
getDbTestR = do 
  infos <- runDB $ selectList [] [LimitTo 10]
  let n = length(infos::[Entity JobResultInfo])
  defaultLayout [whamlet|
    #{show n}
    <ul>
    $forall i <- infos
      <li> #{show i}
    |]
