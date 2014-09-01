module Handler.Registered where

import Import hiding (competitionName, metaCategoryName, categoryName)
import Presenter.Registration
--import Text.Lucius (luciusFile)
--import Text.Hamlet (hamletFile)

getRegisteredR :: Handler Html
getRegisteredR = do
    let comp = Presenter.Registration.tc2014
    defaultLayout $ do
        $(widgetFile "registered")
