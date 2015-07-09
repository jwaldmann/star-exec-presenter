module Handler.DbTest where

import Import
import Presenter.PersistHelper
import Presenter.Model.Entities()

-- slowCpuTimeLimit :: Int
slowCpuTimeLimit = 10

getDbTestR :: JobID -> Handler Html
getDbTestR jid = do
  jobResults <- getPersistJobResults jid
 
  let concepts = createConcepts  $ getStarExecResults jobResults
  let n = length $  jobResults
  defaultLayout [whamlet|
    #{show concepts}
    #{show n}
    <ul>
    $forall jobResult <- getStarExecResults jobResults
      <li> #{show jobResult}
      <li> #{show $ jobResultInfoResult jobResult}
      <li> #{show $ jobResultInfoSolver jobResult}
      <li> #{show $ jobResultInfoCpuTime jobResult}
      <li> #{show $ jobResultInfoWallclockTime jobResult}
    |]



createConcepts :: [JobResultInfo] -> [Bool]
createConcepts = map (isFast . jobResultInfoCpuTime)

isFast :: (Num a, Ord a) => a -> Bool
isFast a = a < slowCpuTimeLimit
