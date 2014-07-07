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


inputForm = renderTable $ JobControl
        <$> areq textField "user" Nothing
        <*> areq passwordField "pass" Nothing 
        <*> areq (radioFieldList [("Termination.q"::T.Text,478),("all.q",1),("all2.q",4)]) "queue" (Just 478)
        <*> areq (radioFieldList [("autotest":: T.Text, 52915)]) "space" (Just 52915)
        <*> areq (radioFieldList [("60"::T.Text, 60),("300", 300), ("900", 900)]) "wallclock_timeout" (Just 60)
        <*> areq (radioFieldList [("1", 1), ("10"::T.Text,10), ("25", 25), ("100", 100)]) "benchmarks_per_category" (Just 25)
        <*> formToAForm ( do 
            e <- askParams 
            return ( FormSuccess $ maybe M.empty id e, [] ) )


benches bs = do Bench b <- bs ; return b
alls bs = do All b <- bs ; return b
hierarchies bs = do Hierarchy b <- bs ; return b

getControlR :: Handler Html
getControlR = do
    (widget, enctype) <- generateFormPost inputForm
    let comp = tc2014
    defaultLayout $(widgetFile "control")

postControlR :: Handler Html
postControlR = do
    ((result,widget), enctype) <- runFormPost inputForm

    let comp = tc2014
    cred <- getLoginCredentials

    mc <- case result of
            FormSuccess input -> 
                if Login (user input) (pass input) == cred
                then do
                    Just [con] <- return $ M.lookup "control" $ env input
                    startjobs ( input { user = "none", pass = "denkste" } ) con 
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


startjobs input con = 
      checkPrefix "cat:"  con (startCat input)
    $ checkPrefix "mc:" con (startMC input)
    $ checkPrefix "comp:" con (startComp input)
    $ return Nothing

checkPrefix :: T.Text -> T.Text -> ( T.Text -> a ) -> a ->  a
checkPrefix s con action next = 
    let (pre, post) = T.splitAt (T.length s) con
    in  if pre == s then action post else next

startComp input t = do
    comp_with_jobs <- pushcomp input tc2014
    let c = convertComp comp_with_jobs
    return $ Just c

startMC input t = do
    let [ mc ] = do 
            mc <- metacategories tc2014
            guard $ metaCategoryName mc == t
            return mc
    mc_with_jobs <- pushmetacat input mc
    let c = S.Competition "Test" [ convertMC mc_with_jobs]
    return $ Just c

startCat input t = do
    let [ cat ] = do 
            mc <- metacategories tc2014
            c <- categories mc
            guard $ categoryName c == t
            return c
    cat_with_jobs <- pushcat input cat    
    let c = S.Competition "Test" [ S.MetaCategory "Test" [ convertC cat_with_jobs]]
    return $ Just c



