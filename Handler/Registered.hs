module Handler.Registered where

import Import
import StarExec.Registration
import Text.Lucius (luciusFile)
import Text.Hamlet (hamletFile)

getRegisteredR :: Handler Html
getRegisteredR = do
    let comp = StarExec.Registration.tc2014
    defaultLayout $ do
        $(widgetFile "registered")

