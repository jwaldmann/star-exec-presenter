module Handler.ShowJobInfo where

import Import
import qualified StarExec.Commands as SEC
import StarExec.JobInfo
import Data.Double.Conversion.Text

getClass :: JobInfo -> Text
getClass info =
    case result info of
        "YES"    -> "success"
        "NO"     -> "success"
        "MAYBE"  -> "success"
        "Error"  -> "danger"
        ""       -> ""
        _        -> "warning"

getShowJobInfoR :: Int -> Handler Html
getShowJobInfoR jobId = do
    con <- SEC.getConnection
    mInfo <- SEC.getJobInfo con jobId
    let jobinfos = case mInfo of
            Just info -> info
            Nothing   -> []
    defaultLayout $ do
        $(widgetFile "show_job_info")
