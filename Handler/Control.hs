{-# language OverloadedStrings #-}

module Handler.Control where


import Import
import StarExec.Registration 
import StarExec.Connection (getLoginCredentials)
import StarExec.Types (Login(..))
import StarExec.Persist
import StarExec.Commands (Job(..))
import qualified StarExec.CompetitionResults as SCR

import Yesod.Auth

import Control.Monad ( guard, forever )
import qualified StarExec.Types as S
import qualified Data.Text as T
import Data.Time.Clock

import Control.Job

import qualified Data.Map.Strict as M
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Control.Exception.Base


inputForm = renderTable $ JobControl
        <$> areq checkBoxField "is public" (Just False)
        <*> areq (radioFieldList [("Termination.q"::T.Text,478),("all.q",1),("all2.q",4)]) "queue" (Just 478)
        <*> areq (radioFieldList [("autotest":: T.Text, 52915)]) "space" (Just 52915)
        <*> areq (radioFieldList [("60"::T.Text, 60),("300", 300), ("900", 900)]) "wallclock_timeout" (Just 60)
        <*> areq (radioFieldList [("1", 1), ("10"::T.Text,10), ("25", 25), ("100", 100)]) 
                 "family_lower_bound (selection parameter a)" (Just 10)
        <*> areq (radioFieldList [("1", 1), ("10"::T.Text,10), ("25", 25), ("100", 100)]) 
                 "family_upper_bound (selection parameter b)" (Just 100)
        <*> areq (radioFieldList [("0.1", 0.1), ("0.3"::T.Text,0.3), ("0.5", 0.5), ("1.0", 1.0)]) 
                 "family_factor (selection parameter c)" (Just 0.5)
        <*> formToAForm ( do 
            e <- askParams 
            return ( FormSuccess $ maybe M.empty id e, [] ) )


benches bs = do Bench b <- bs ; return b
alls bs = do All b <- bs ; return b
hierarchies bs = do Hierarchy b <- bs ; return b

getControlR :: Handler Html
getControlR = do
    maid <- maybeAuthId
    (widget, enctype) <- generateFormPost inputForm
    let comp = tc2014
    defaultLayout $(widgetFile "control")

start_worker comp = do
    app <- getYesod

    mSink <- lift $ atomically $ do
      crc <- readTVar $ compResultsCache app
      case M.lookup (S.getCompetitionName comp) crc of
          Nothing -> do
              sink <- newTVar Nothing
              modifyTVar' (compResultsCache app) $ M.insert (S.getCompetitionName comp) sink
              return $ Just sink
          Just entry -> do
              return Nothing
    case mSink of
        Nothing -> return ()
        Just sink -> do
            lift $ putStrLn $ "start worker for " ++ ( T.unpack $ S.getCompetitionName comp )
            forkHandler error_handler $ forever $ do -- should finish when complete
                compResults <- SCR.getCompetitionResults comp
                lift $ atomically $ writeTVar sink $ Just compResults
                lift $ threadDelay $ 30 * 10^6

error_handler :: SomeException -> Handler ()
error_handler e = lift $ do
    putStrLn $ "ignoring exception:"
    putStrLn $ show e


postControlR :: Handler Html
postControlR = do
    maid <- maybeAuthId
    ((result,widget), enctype) <- runFormPost inputForm

    let comp = tc2014
        public = case result of
                  FormSuccess input -> isPublic input
                  _ -> False
    mc <- case result of
            FormSuccess input -> do
                    Just [con] <- return $ M.lookup "control" $ env input
                    startjobs input con 
            _ -> return Nothing
    mKey <- case mc of 
              Nothing -> return Nothing
              Just c -> do
                  now <- liftIO getCurrentTime
                  let competition = ( timed now c ) 
                  key <- runDB $ insert $ CompetitionInfo competition now public
                  start_worker competition
                  return $ Just key
    defaultLayout $ do
        [whamlet|
          <h2>Result of previous command
          $maybe key <- mKey
            jobs started, <a href=@{CompetitionR key}>output</a>
          $nothing
            could not start jobs
        |] 
        $(widgetFile "control")

startjobs :: JobControl -> Text -> Handler (Maybe S.Competition)
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
    let S.Competition name mcs = convertComp comp_with_jobs
        n = params input t 
        c = S.Competition n mcs
    return $ Just c

startMC input t = do
    let [ mc ] = do 
            mc <- metacategories tc2014
            guard $ metaCategoryName mc == t
            return mc
    mc_with_jobs <- pushmetacat input mc
    let n = params input t
        c = S.Competition n [ convertMC mc_with_jobs]
    return $ Just c

startCat input t = do
    let [ cat ] = do 
            mc <- metacategories tc2014
            c <- categories mc
            guard $ categoryName c == t
            return c
    cat_with_jobs <- pushcat input cat    
    let n = params input t
        c = S.Competition n [ S.MetaCategory n [ convertC cat_with_jobs]]
    return $ Just c

params conf t = T.unwords
    [ "Test", t
    , "wc", "=", T.pack $ show $ wallclock conf
    , "a", "=", T.pack $ show $ family_lower_bound conf
    , "b", "=", T.pack $ show $ family_upper_bound conf
    , "c", "=", T.pack $ show $ family_factor conf
    ]


