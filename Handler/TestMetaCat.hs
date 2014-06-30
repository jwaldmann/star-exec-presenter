module Handler.TestMetaCat where

import Import

import qualified Data.Text as T

import StarExec.Registration 
import StarExec.Commands (pushJobXml, Job (..))
import StarExec.Connection (getConnection)
import StarExec.Types (JobIds(..))

import Handler.TestCat ( pushmetacat )

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

    defaultLayout $ do
        setTitle "testMetaCat"
        [whamlet|
<pre>
   #{show mc_with_jobs}

    
|]
