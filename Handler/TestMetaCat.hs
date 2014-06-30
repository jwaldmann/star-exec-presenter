module Handler.TestMetaCat where

import Import

import qualified Data.Text as T

import StarExec.Registration 
import StarExec.Commands (pushJobXml, Job (..))
import StarExec.Connection (getConnection)
import StarExec.Types (JobIds(..))
import qualified StarExec.Types as S

import Handler.TestCat ( pushmetacat, convertMC )

import Data.Time.Clock
import Control.Monad ( guard, forM )
import Data.Char (isAlphaNum)

autotest_spaceId = 52915 :: Int
termination_queueId = 478 :: Int

getTestMetaCatR :: Text -> Handler Html
getTestMetaCatR t = do
    let [ mc ] = do 
            mc <- metacategories tc2014
            guard $ metaCategoryName mc == t
            return mc
    mc_with_jobs <- pushmetacat mc

    let c = S.Competition "Test" [ convertMC mc_with_jobs]

    defaultLayout $ do
        setTitle "testMetaCat"
        [whamlet|<a href=@{CompetitionWithConfigR c}>test output</a>|]

