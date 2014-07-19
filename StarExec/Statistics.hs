module StarExec.Statistics where

import Prelude
import Model
import StarExec.Types 
import Control.Applicative
import Data.Monoid
import Text.Blaze
import Text.Hamlet
import Data.Text
import Data.Time.Clock
import Data.Double.Conversion.Text (toFixed)

data Statistics = Statistics 
    { complete :: ! Bool
    , startTime :: ! (Maybe UTCTime), finishTime :: ! (Maybe UTCTime)
    , pairs :: ! Int, pairsCompleted :: ! Int
    , cpu :: ! Double, wallclock :: ! Double
    } deriving Show

jobStat :: JobResultInfo -> Statistics
jobStat i = 
    let done = jobResultInfoStatus i == JobResultComplete
    in  Statistics
        { complete = done
        , startTime = Nothing , finishTime = Nothing -- FIXME
        , pairs = 1 , pairsCompleted = if done then 1 else 0
        , cpu = jobResultInfoCpuTime i
        , wallclock = jobResultInfoWallclockTime i
        }

instance ToMarkup Statistics where
    toMarkup s = [shamlet|    
      <span>
        $if complete s
          completed, 
        $else
          running, 
        #{pairsCompleted s} 
        $if not (complete s)
           of #{pairs s} #
        pairs, 
        #{toFixed 1 $ cpu s} cpu, #{toFixed 1 $ wallclock s} wall
      |]


instance Monoid Statistics where
    mempty = Statistics 
        { complete = True , startTime = Nothing, finishTime = Nothing
        , pairs = 0, pairsCompleted = 0
        , cpu = 0.0, wallclock = 0.0
        }
    mappend s t = Statistics
        { complete = complete s && complete t
        , pairs = pairs s + pairs t , pairsCompleted = pairsCompleted s + pairsCompleted t
        , startTime = min <$> startTime s <*> startTime t
        , finishTime = min <$> finishTime s <*> finishTime t
        , cpu = cpu s + cpu t, wallclock = wallclock s + wallclock t
        }

