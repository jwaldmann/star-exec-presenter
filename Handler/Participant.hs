module Handler.Participant where

import Import
import Presenter.Registration

getParticipantR :: Year -> Text -> Handler Html
getParticipantR year name = do
  defaultLayout $ do
    [whamlet|
     <h1>Registration Template in #{show year} for #{name} 
     <pre>#{show $ extract year name}
    |]
