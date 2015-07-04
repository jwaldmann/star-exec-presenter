module Handler.Registered where

import Import hiding (competitionName, metaCategoryName, categoryName)
import Presenter.Registration
--import Text.Lucius (luciusFile)
--import Text.Hamlet (hamletFile)

getRegisteredR :: Year -> Handler Html
getRegisteredR year = do
    let comp = Presenter.Registration.the_competition year
    defaultLayout $ do
        $(widgetFile "registered")
