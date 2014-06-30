module Handler.TestComp where

import Import

import qualified Data.Text as T

import StarExec.Registration 
import StarExec.Commands (pushJobXml, Job (..))
import StarExec.Connection (getConnection)
import StarExec.Types (JobIds(..))


import Handler.TestCat ( pushcomp, convertComp )

import Data.Time.Clock
import Control.Monad ( guard, forM )
import Data.Char (isAlphaNum)



getTestCompR :: Text -> Handler Html
getTestCompR t = do
    comp_with_jobs <- pushcomp tc2014

    let c = convertComp comp_with_jobs

    defaultLayout $ do
        [whamlet|<a href=@{CompetitionWithConfigR c}>test output</a>|]

