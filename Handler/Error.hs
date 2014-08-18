module Handler.Error where

import Import
import Presenter.RouteTypes (ErrorID (..))

getErrorR :: ErrorID -> Handler Html
getErrorR LoginError = error "Wrong Login-Credentials?"
getErrorR _ = error "Not yet implemented: getErrorR"
