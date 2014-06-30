module Handler.TestCat where

import Import

import qualified Data.Text as T

import StarExec.Registration 
import StarExec.Commands (pushJobXml, Job (..))
import StarExec.Connection (getConnection)
import StarExec.Types (JobIds(..))

import Data.Time.Clock
import Control.Monad ( guard, forM )
import Data.Char (isAlphaNum)

autotest_spaceId = 52915 :: Int
termination_queueId = 478 :: Int

pushcat cat = do
    mis <- pushcatjobs cat
    return $ cat { contents = (contents cat, mis) }

pushmetacat mc = do
    cs <- forM (categories mc) pushcat
    return $ mc { categories = cs }

pushcomp c = do
    mcs <- forM (metacategories c) pushmetacat
    return $ c { metacategories = mcs }

pushcatjobs cat = do
    let ci = contents cat
    now <- liftIO getCurrentTime
    let repair = T.pack . map ( \ c -> if isAlphaNum c then c else ' ' )
    let job = Job 
         { postproc_id = postproc ci
         , description = "getTestCatR"
         , job_name = repair $ "getTestCatR " ++ show now
         , queue_id = termination_queueId
         , mem_limit = 128.0
         , wallclock_timeout = 60
         , cpu_timeout = 240
         , start_paused = False
         , jobpairs = do 
               Bench { bench = b } <- benchmarks ci
               Participant { solver_config = Just (s,c) } <- participants ci
               return ( b, c )
         }
    con <- getConnection
    pushJobXml con autotest_spaceId [ job ]


getTestCatR :: Text -> Handler Html
getTestCatR t = do
    let [ cat ] = do 
            mc <- metacategories tc2014
            c <- categories mc
            guard $ categoryName c == t
            return c
    mi <- pushcatjobs cat

    defaultLayout $ do
        setTitle "testCat"
        [whamlet|
<pre>
        getTestCatR: #{t}  
        pushjobXML, answer is #{show mi}
<a href=@{ShowManyJobResultsR (JobIds (maybe [] id mi))}> job results
    
|]
