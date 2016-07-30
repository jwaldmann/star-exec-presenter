module Handler.Participant where

import Import
import Presenter.Registration

getParticipantR :: Year -> Text -> Handler Html
getParticipantR year name = do
  defaultLayout $ do
    [whamlet|
     <h1>Registration Template in #{show year} for #{name} 
     if you want to update your submission, send email in exactly this form:
     <pre>#{show $ extract year name}
     solver_config must be Just (sp,so,co) where
     <ul>
        <li>sp = starexec id of space that contains the solver
        <li>so = starexec id of solver
        <li>co = starexec id of configuration
     The starexec space must be readable for the competition organizer.
    |]
