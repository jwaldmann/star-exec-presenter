module Handler.Error where

import Import
import ErrorID

getErrorR :: ErrorID -> Handler Html
getErrorR Login = error "Wrong Login-Credentials?"
getErrorR err = error "Not yet implemented: getErrorR"
