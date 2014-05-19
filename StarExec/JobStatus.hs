module StarExec.JobStatus where

import Data.Text
import Prelude
import Database.Persist.TH
import Text.Blaze
import Text.Blaze.Internal

data JobStatus = Complete | Incomplete Text
    deriving (Show, Read, Eq)
derivePersistField "JobStatus"

instance ToMarkup JobStatus where
    toMarkup = string . show
    preEscapedToMarkup = preEscapedString . show
