module StarExec.SolverResult where

import Data.Text
import Data.Text.Encoding
import Prelude
import Database.Persist.TH
import Data.Csv
import Control.Applicative
import Text.Blaze
import Text.Blaze.Internal

data SolverResult = YES | NO | CERTIFIED | MAYBE | ERROR | OTHER Text
    deriving (Show, Read, Eq)
derivePersistField "SolverResult"

instance FromField SolverResult where
    parseField result = parseResult s
        where
            s = toLower $ decodeUtf8 result
            parseResult r
                | r == "yes"        = pure YES
                | r == "no"         = pure NO
                | r == "maybe"      = pure MAYBE
                | r == "certified"  = pure CERTIFIED
                | r == "error"      = pure ERROR
                | otherwise         = pure $ OTHER $ decodeUtf8 result

instance ToMarkup SolverResult where
    toMarkup = string . show
    preEscapedToMarkup = preEscapedString . show
    