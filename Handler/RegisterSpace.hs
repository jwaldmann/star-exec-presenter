module Handler.RegisterSpace where

import Import

postRegisterSpaceR :: Handler Html
postRegisterSpaceR = do
    spaceID <- runInputPost $ ireq intField "spaceId"
    redirect UserR
