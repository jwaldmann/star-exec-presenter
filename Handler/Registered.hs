module Handler.Registered where

import Import hiding (competitionName, metaCategoryName, categoryName)
import Presenter.Registration

getRegisteredR :: Year -> Handler Html
getRegisteredR year = do
    let comp = Presenter.Registration.the_competition year
        -- configs build in 2014 have low numbers
        oldconfig c = c < 24757
        partclass :: (Int,Int,Int) -> Text
        partclass (p,s,c) = if oldconfig c then "oldconf" else "conf"
    defaultLayout $(widgetFile "registered")
