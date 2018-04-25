module Handler.Registered where

import Import hiding (competitionName, metaCategoryName, categoryName)
import Presenter.Registration

-- old configs have low numbers
oldconfig c = c < 165939 

getRegisteredR :: Year -> Handler Html
getRegisteredR year = do
    let comp = Presenter.Registration.the_competition year
        partclass :: (Int,Int,Int) -> Text
        partclass (p,s,c) = if oldconfig c then "oldconf" else "conf"
    defaultLayout $(widgetFile "registered")
