-- | cf. https://github.com/StarExec/StarExec/blob/fb1/src/org/starexec/command/C.java
-- TODO: use exactly their names here

module Presenter.StarExec.Urls where

import Data.ByteString
import Prelude (String, ($))
import qualified Data.List as List
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Search as BSS

starexec_base = "https://www.starexec.org/starexec/"
home = "secure/index.jsp"
login = "secure/j_security_check"
logout = "services/session/logout"

logged_in = "services/session/logged-in"

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

pairStdoutPath :: ByteString
pairStdoutPath = "starexec/services/jobs/pairs/{pairId}/stdout/1"

pairLogPath :: ByteString
pairLogPath = "starexec/services/jobs/pairs/{pairId}/log"

jobInfoPath :: ByteString
jobInfoPath = "starexec/secure/details/job.jsp"

solverInfoPath :: ByteString
solverInfoPath = "starexec/secure/details/solver.jsp"

configPath :: ByteString
configPath = "starexec/secure/details/configuration.jsp"

benchmarkInfoPath :: ByteString
benchmarkInfoPath = "starexec/secure/details/benchmark.jsp"

benchmarkPath :: ByteString
benchmarkPath = "starexec/services/benchmarks/{bmId}/contents"

postProcPath :: ByteString
postProcPath = "starexec/secure/edit/processor.jsp"

getURL :: ByteString -> [(String, String)] -> ByteString
getURL url patterns = List.foldl' (\path (pattern, sub) ->
    BSL.toStrict $ BSS.replace
      (BSC.pack pattern)
      (BSC.pack sub)
      path
  ) url patterns

downloadPath :: ByteString
downloadPath = "starexec/secure/download"

pushjobxmlPath :: ByteString
pushjobxmlPath = "starexec/secure/upload/jobXML"               

pausePath :: ByteString
pausePath = "starexec/services/pause/job/{id}"

resumePath :: ByteString
resumePath = "starexec/services/resume/job/{id}"

rerunPath :: ByteString
rerunPath = "starexec/services/jobs/rerunallpairs/{id}"

addSolverPath :: ByteString
addSolverPath = "starexec/services/spaces/{spaceId}/add/solver"

removeSolverPath :: ByteString
removeSolverPath = "starexec/services/remove/solver/{spaceId}"

addJobPath :: ByteString
addJobPath = "starexec/secure/add/job"

addSpacePath :: ByteString
addSpacePath = "starexec/secure/add/space"
