module Handler.Control where


import Import
import StarExec.Registration
import Text.Lucius (luciusFile)
import Text.Hamlet (hamletFile)

getControlR :: Handler Html
getControlR = do
    let comp = StarExec.Registration.tc2014
    defaultLayout 
        $(widgetFile "control")

