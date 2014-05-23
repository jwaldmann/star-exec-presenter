module StarExec.Urls where

import Data.ByteString
import Prelude (String, ($))
import qualified Data.List as List
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Search as BSS

starExecUrl :: String
starExecUrl = "https://www.starexec.org/"

starExecSpacesPath :: ByteString
starExecSpacesPath = "https://www.starexec.org/starexec/secure/explore/spaces.jsp"

indexPath :: ByteString
indexPath = "starexec/secure/index.jsp"

loginPath :: ByteString
loginPath = "starexec/secure/j_security_check"

logoutPath :: ByteString
logoutPath = "starexec/services/session/logout"

spacesPath :: ByteString
spacesPath = "starexec/secure/explore/spaces.jsp"

userIDPath :: ByteString
userIDPath = "starexec/services/users/getid"

primPath :: ByteString
primPath = "starexec/services/space/{id}/{type}/pagination"

getPrimURL :: ByteString -> [(String, String)] -> ByteString
getPrimURL url patterns = List.foldl' (\path (pattern, sub) ->
    BSL.toStrict $ BSS.replace
      (BSC.pack pattern)
      (BSC.pack sub)
      path
  ) url patterns

downloadPath :: ByteString
downloadPath = "starexec/secure/download"
