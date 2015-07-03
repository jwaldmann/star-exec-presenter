module Presenter.Model.Defaults where

import Prelude
import Model
import Presenter.Model.StarExec
import Data.Time.Calendar
import Data.Time.Clock

baseDate :: UTCTime
baseDate = UTCTime
  { utctDay = fromGregorian 1970 1 1
  , utctDayTime = secondsToDiffTime 0
  }

defaultJobInfo :: JobInfo
defaultJobInfo = JobInfo
  { jobInfoStarExecId = 0
  , jobInfoName = ""
  , jobInfoStatus = Started
  , jobInfoDate = ""
  , jobInfoPreProc = ""
  , jobInfoPostProc = ""
  , jobInfoIsComplexity = False
  , jobInfoIsPublic = True
  , jobInfoStartDate = baseDate
  , jobInfoFinishDate = Nothing
  , jobInfoLastUpdate = baseDate
  }
