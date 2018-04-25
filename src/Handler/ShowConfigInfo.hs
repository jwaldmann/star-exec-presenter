module Handler.ShowConfigInfo where

import Import

getShowConfigInfoR :: ConfigID -> Handler Html
getShowConfigInfoR sid@(StarExecConfigID _id) = do
  defaultLayout $ do
    [whamlet|
         <h1>Config-Info #{show sid}
         <p>(not implemented)
         <p>view original config-info on star-exec: <a href="https://www.starexec.org/starexec/secure/details/configuration.jsp?id=#{show _id}">#{show _id}</a>
    |]

{-
  (QueryResult qStatus mConfigInfo) <- queryConfigInfo sid
  defaultLayout $ do
    case qStatus of
      Latest -> return ()
      Pending _ -> insertWidgetMetaRefresh
    [whamlet|
       $maybe configInfo <- mConfigInfo
         <h1>Config-Info of #{toConfigName configInfo}
         view original config-info on star-exec: <a href="https://www.starexec.org/starexec/secure/details/configuration.jsp?id=#{show _id}">#{show _id}</a>
       $nothing
         <h1>No config with id #{show _id} found
    |]
-}
