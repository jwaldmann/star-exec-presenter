module Handler.Pause where

import Import
import Yesod.Auth

postPauseR :: JobIds -> Handler Html
postPauseR jobIds = do
  maid <- maybeAuthId
  error "Not yet implemented: postPauseR"
