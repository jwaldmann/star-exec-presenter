module Handler.Resume where

import Import
import Yesod.Auth

postResumeR :: JobIds -> Handler Html
postResumeR jobIds = do
  maid <- maybeAuthId
  error "Not yet implemented: postResumeR"
