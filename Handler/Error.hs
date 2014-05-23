module Handler.Error where

import Import
import StarExec.Types (ErrorID (..))

getErrorR :: ErrorID -> Handler Html
getErrorR Login = error "Wrong Login-Credentials?"
getErrorR err = error "Not yet implemented: getErrorR"
