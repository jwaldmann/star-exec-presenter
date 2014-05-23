module Jobs where

import Prelude
import Data.Text
import Data.Maybe
import qualified Data.List
import StarExec.Types
import StarExec.Connection
import StarExec.Commands

data JobInfo = JobInfo
    { id :: Int
    , status :: JobStatus
    }

hasJob :: [(Text, Text)] -> Int -> Bool
hasJob [] _ = False

getJob :: [(Text, Text)] -> Int -> JobInfo

checkSpace :: (Maybe JobInfo) -> 
checkSpace mJobInfo@(Just jobInfo) _ = mJobInfo
checkSpace _ _ = Nothing

findJobInHierarchy :: StarExecConnecton -> Int -> Int -> Maybe JobInfo
findJobInHierarchy con spaceId jobId = do
    mJobList <- listPrim con spaceId Jobs
    let jobList = fromMaybe [] mJobList
    if hasJob jobList jobId
        then return $ getJob jobList jobId
        else do
            mSpaceList <- listPrim con spaceId Spaces
            let spaceList = fromMaybe [] mSpaceList
            if null spaceList
                then return Nothing
                else List.foldl' checkSpace Nothing spaceList

findJob :: StarExecConnecton -> Int -> Maybe JobInfo
findJob con id = findJobInHierarchy con 1 id
