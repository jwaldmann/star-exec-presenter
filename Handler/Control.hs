{-# language OverloadedStrings #-}

module Handler.Control where


import Import
import StarExec.Registration 
import StarExec.Connection (getLoginCredentials)
import StarExec.Types (Login(..))
import StarExec.Persist
import StarExec.Commands (Job(..))

import Control.Monad ( guard )
import qualified StarExec.Types as S
import qualified Data.Text as T
import Data.Time.Clock

import Control.Job

import qualified Data.Map as M

controlForm :: Form Login
controlForm = renderDivs $ Login
        <$> areq textField "user" Nothing
        <*> areq passwordField "pass" Nothing 

{-

jobstartForm :: Form Job
jobstartForm = renderDivs $ Job 
        <$> pure 0
        <*> pure "description (set by starter)"
        <*> pure "job_name (set by starter)"
        <*> areq (radioFieldList [("Termination.q",478),("all.q",1),("all2.q",4)]) "queue" (Just 478)
        <*> areq (radioFieldList [("128", 128), ("256", 256)]) "mem_limit" (Just 128)
        <*> areq (radioFieldList [("60", 60)]) "wallclock_timeout" (Just 60)
        <*> areq (radioFieldList [("240", 240)]) "cpu_timeout" (Just 240)
        <*> areq (radioFieldList [("true", True), ("false", False)]) "start_paused" (Just False)
        <*> pure []
        <*> pure Nothing

-}

benches bs = do Bench b <- bs ; return b
alls bs = do All b <- bs ; return b
hierarchies bs = do Hierarchy b <- bs ; return b

getControlR :: Handler Html
getControlR = do
    (widget,enctype) <- generateFormPost $ controlForm
    let comp = tc2014
    defaultLayout $(widgetFile "control")

postControlR :: Handler Html
postControlR = do
    ((result,widget),enctype) <- runFormPost $ \ mu -> do
        (res,widg) <- controlForm mu        
        Just env <- askParams -- FIXME
        return (fmap ((,) env) res , widg )
    let comp = tc2014
    cred <- getLoginCredentials

    mc <- case result of
            FormSuccess (env, s) -> 
                if s == cred
                then do
                    Just [con] <- return $ M.lookup "control" env
                    startjobs con
                else return Nothing
            _ -> return Nothing
    case mc of 
        Nothing -> return ()
        Just c -> do
            now <- liftIO getCurrentTime
            runDB $ insert $ CompetitionInfo ( timed now c ) now
            return ()
    defaultLayout $ do
        [whamlet|<h2>Result of previous command
$maybe c <- mc
    jobs started, <a href=@{CompetitionWithConfigR c}>output</a>
$nothing
    could not start jobs
|] 
        $(widgetFile "control")

startjobs con = 
      checkPrefix "cat:"  con startCat
    $ checkPrefix "mc:" con startMC
    $ checkPrefix "comp:" con startComp
    $ return Nothing

checkPrefix :: T.Text -> T.Text -> ( T.Text -> a ) -> a ->  a
checkPrefix s con action next = 
    let (pre, post) = T.splitAt (T.length s) con
    in  if pre == s then action post else next

startComp t = do
    comp_with_jobs <- pushcomp tc2014
    let c = convertComp comp_with_jobs
    return $ Just c

startMC t = do
    let [ mc ] = do 
            mc <- metacategories tc2014
            guard $ metaCategoryName mc == t
            return mc
    mc_with_jobs <- pushmetacat mc
    let c = S.Competition "Test" [ convertMC mc_with_jobs]
    return $ Just c

startCat t = do
    let [ cat ] = do 
            mc <- metacategories tc2014
            c <- categories mc
            guard $ categoryName c == t
            return c
    cat_with_jobs <- pushcat cat    
    let c = S.Competition "Test" [ S.MetaCategory "Test" [ convertC cat_with_jobs]]
    return $ Just c



