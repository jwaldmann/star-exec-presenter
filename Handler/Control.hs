{-# language OverloadedStrings #-}

module Handler.Control where


import Import
import StarExec.Registration 
import StarExec.Connection (getLoginCredentials)
import StarExec.Types (Login(..))
import Control.Monad ( guard )
import qualified StarExec.Types as S
import qualified Data.Text as T

import Control.Job

import qualified Data.Map as M

controlForm :: Form Login
controlForm = renderDivs $ Login
        <$> areq textField "user" Nothing
        <*> areq passwordField "pass" Nothing 

getControlR :: Handler Html
getControlR = do
    (widget,enctype) <- generateFormPost $ controlForm
    let comp = tc2014
    defaultLayout 
        $(widgetFile "control")

postControlR :: Handler Html
postControlR = do
    liftIO $ putStrLn "========= yay"
    ((result,widget),enctype) <- runFormPost $ \ mu -> do
        (res,widg) <- controlForm mu        
        Just e <- askParams -- FIXME
        Just [con] <- return $ M.lookup "control" e
        return (fmap ((,) con) res , widg )
    let comp = tc2014
    cred <- getLoginCredentials
    mc <- case result of
            FormSuccess (con, s) -> 
                if s == cred
                then startjobs con
                else return Nothing
            _ -> return Nothing
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
--  $ checkPrefix "comp:" con startComp
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



