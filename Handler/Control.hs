module Handler.Control where

import Import

import Yesod.Auth

import qualified Presenter.Registration as R
import Presenter.Control.Job
import Presenter.STM

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Time.Clock
import Control.Monad ( guard )

inputForm = renderTable $ JobControl
        <$> areq checkBoxField "is public" (Just False)
        <*> areq (radioFieldList [("Competition (at least 2 participants)"::T.Text,SelectionCompetition),("Demonstration (1 participant)",SelectionDemonstration)]) "categories" (Just SelectionCompetition)
        <*> areq (radioFieldList [("Termination.q"::T.Text,478),("TerminationTest.q",30597),("all.q",1),("all2.q",4)]) "queue" (Just 478)
        <*> areq (radioFieldList [("autotest":: T.Text, 52915)]) "space" (Just 52915)
        <*> areq (radioFieldList [("60"::T.Text, 60),("300", 300), ("900", 900)]) "wallclock_timeout" (Just 60)
        <*> areq (radioFieldList [("1", 1), ("10"::T.Text,10), ("25", 25), ("100", 100)]) 
                 "family_lower_bound (selection parameter a)" (Just 10)
        <*> areq (radioFieldList [("1", 1), ("10"::T.Text,10), ("25", 25), ("100", 100),("250",250),("1000",1000)]) 
                 "family_upper_bound (selection parameter b)" (Just 100)
        <*> areq (radioFieldList [("0.1", 0.1), ("0.3"::T.Text,0.3), ("0.5", 0.5), ("1.0", 1.0)]) 
                 "family_factor (selection parameter c)" (Just 0.5)
        <*> formToAForm ( do 
            e <- askParams 
            return ( FormSuccess $ maybe M.empty id e, [] ) )

benches :: Monad m => m R.Benchmark_Source -> m Int
benches bs = do R.Bench b <- bs ; return b

alls :: Monad m => m R.Benchmark_Source -> m Int
alls bs = do R.All b <- bs ; return b

hierarchies :: Monad m => m R.Benchmark_Source -> m Int
hierarchies bs = do R.Hierarchy b <- bs ; return b

getControlR :: Handler Html
getControlR = do
  maid <- maybeAuthId
  (widget, enctype) <- generateFormPost inputForm
  let comp = R.the_competition
  defaultLayout $(widgetFile "control")

postControlR :: Handler Html
postControlR = do
  maid <- maybeAuthId
  ((result, widget), enctype) <- runFormPost inputForm

  let comp = R.the_competition
      public = case result of 
                  FormSuccess input-> isPublic input
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
              startWorker competition
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

startjobs :: JobControl -> Text -> Handler (Maybe Competition)
startjobs input con = 
      checkPrefix "cat:"  con (startCat input)
    $ checkPrefix "mc:" con (startMC input)
    $ checkPrefix "comp:" con (startComp input)
    $ return Nothing

checkPrefix :: T.Text -> T.Text -> ( T.Text -> a ) -> a ->  a
checkPrefix s con action next = 
    let (pre, post) = T.splitAt (T.length s) con
    in  if pre == s then action post else next

select :: JobControl -> R.Competition R.Catinfo -> R.Competition R.Catinfo
select input comp = case selection input of
    SelectionCompetition -> 
        comp { R.metacategories = map ( \ mc -> mc { R.categories = R.full_categories mc } ) 
                              $ R.metacategories comp }
    SelectionDemonstration -> 
        comp { R.metacategories = map ( \ mc -> mc { R.categories = R.demonstration_categories mc } ) 
                              $ R.metacategories comp }

startCat :: JobControl -> Name -> Handler (Maybe Competition)
startCat input t = do
    let cats = do 
            mc <- R.metacategories $ select input R.the_competition
            c <- R.categories mc
            guard $ R.categoryName c == t
            return c
    case cats of
        [ cat ] -> do
            cat_with_jobs <- pushcat input cat    
            let m = params input t
                c = Competition m [ MetaCategory (metaToName m) [ convertC cat_with_jobs]]
            return $ Just c
        _ -> return Nothing

startMC :: JobControl -> Name -> Handler (Maybe Competition)
startMC input t = do
    let mcs = do 
            mc <- R.metacategories $ select input R.the_competition
            guard $ R.metaCategoryName mc == t
            return mc
    case mcs of
        [ mc ] -> do
            mc_with_jobs <- pushmetacat input mc
            let m = params input t
                c = Competition m [ convertMC mc_with_jobs]
            return $ Just c
        _ -> return Nothing

startComp :: JobControl -> Text -> Handler (Maybe Competition)
startComp input t = do
    comp_with_jobs <- pushcomp input $ select input R.the_competition
    let Competition name mcs = convertComp comp_with_jobs 
        m = params input t 
        c = Competition m mcs
    return $ Just c

params :: JobControl -> Text -> CompetitionMeta
params conf t = CompetitionMeta
  { getMetaName = T.append t $ case selection conf of
        SelectionCompetition -> T.empty
        SelectionDemonstration -> " (Demonstration)"
  , getMetaDescription =
      T.unwords [ "wc", "=", T.pack $ show $ wallclock conf
                , "a", "=", T.pack $ show $ family_lower_bound conf
                , "b", "=", T.pack $ show $ family_upper_bound conf
                , "c", "=", T.pack $ show $ family_factor conf
                ]
  }

metaToName :: CompetitionMeta -> Name
metaToName meta = getMetaName meta
