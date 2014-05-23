module StarExec.Prims where

import Import
import Prelude
import Data.Maybe
import Data.Char
import Data.Aeson
import Data.Either
import Text.XML
import Text.XML.Cursor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import StarExec.Types
import StarExec.Urls
import StarExec.Connection
import Network.HTTP.Conduit
import Network.HTTP.Types.Header

type PrimIdent = (Text, Text)

getPostData :: Int -> [(BS.ByteString, BS.ByteString)]
--getPostData :: Int -> [(String, String)]
getPostData columns =
  [ ("sEcho", "1")
  , ("iColumns", BSC.pack $ show columns)
  , ("sColumns", "")
  , ("iDisplayStart", "0")
  , ("iDisplayLength", "2147483647")
  , ("iSortCol_0", "0")
  , ("sSearch", "")
  , ("sSortDir_0", "asc") ]

listPrim :: ( MonadHandler m ) =>
  StarExecConnection -> Int -> StarExecListType -> m [Either String PrimInfo]
listPrim (sec, man) primID primType = do
  let columns = getColumns primType
      sType = map toLower $ show primType
      reqPath = getPrimURL
              primPath
              [ ("{id}", show primID)
              , ("{type}", sType) ]
      (+>) = BS.append
      postData = List.foldl' (\bs (k, v) ->
          bs +> "&" +> k +> "=" +> v
        ) "" $ getPostData columns
      req = sec {
                  method = "POST"
                , path = reqPath
                , requestHeaders = [ (
                    hContentType,
                    "application/x-www-form-urlencoded"
                  ) ]
                , requestBody = RequestBodyBS $ BS.tail postData
                }
  resp <- sendRequest (req, man)
  let jsonObj = eitherDecode
        $ responseBody resp :: (Either String ListPrimResult)
  case jsonObj of
    Right result ->
      return $ parsePrimInfos primType $ aaData result
    Left msg -> do
      liftIO $ print $ show msg
      return []

fromEither :: [Either String PrimInfo] -> [PrimInfo]
fromEither = rights

eitherInfo :: (Either String a) -> (a -> b) -> Either String b
eitherInfo (Right info) dataConst = Right $ dataConst info
eitherInfo (Left msg) _ = Left msg

parsePrimInfo :: StarExecListType -> [Text] -> Either String PrimInfo
parsePrimInfo Jobs       info = eitherInfo (getJobInfoFromTexts info) PrimJobInfo
parsePrimInfo Spaces     info = eitherInfo (getSpaceInfoFromTexts info) PrimSpaceInfo
parsePrimInfo Benchmarks info = eitherInfo (getBenchmarkInfoFromTexts info) PrimBenchmarkInfo
parsePrimInfo Users      info = eitherInfo (getUserInfoFromTexts info) PrimUserInfo
parsePrimInfo Solvers    info = eitherInfo (getSolverInfoFromTexts info) PrimSolverInfo

parsePrimInfos :: StarExecListType -> [[Text]] -> [Either String PrimInfo]
parsePrimInfos primType infos = map (parsePrimInfo primType) infos

getInput :: Cursor -> Cursor
getInput c = head $ descendant c >>= element "input"

getNodeValue :: Cursor -> Text
getNodeValue = head . attribute "value"

getAnchor :: Cursor -> Cursor
getAnchor c = head $ descendant c >>= element "a" >>= child

wrapHtml :: T.Text -> T.Text
wrapHtml html = 
  let (+>) = T.append
  in "<div>" +> html +> "</div>"

getCursor :: Text -> Cursor
getCursor = fromDocument . parseText_ def . TL.fromStrict

parsePrimIdAndName :: Text -> (Int, Text)
parsePrimIdAndName text =
    let cursor = getCursor $ wrapHtml text
        input = getInput cursor
        anchor = getAnchor cursor
        inputID = getNodeValue input
        anchorContent = head $ content anchor
    in (read $ T.unpack inputID, anchorContent)

getStatus :: Text -> JobStatus
getStatus "complete" = Complete
getStatus _ = Incomplete

parseDouble :: Text -> Double
parseDouble text =
    let cursor = getCursor text
        childNode = head $ descendant cursor
        childContent = head $ content childNode
        int = T.reverse $ T.drop 2 $ T.reverse childContent
    in (fromIntegral $ read $ T.unpack int) / 100

parseInt :: Text -> Int
parseInt text =
    let cursor = getCursor text
        childNode = head $ descendant cursor
        childContent = head $ content childNode
    in read $ T.unpack childContent

parseMail :: Text -> Text
parseMail text =
    let cursor = getCursor text
        childContent = head $ descendant cursor
    in head $ content childContent

getJobInfoFromTexts :: [Text] -> Either String JobInfo
getJobInfoFromTexts
  (idAndName:status:pairsCompleted:pairsNum:pairsFailed:date:rest) =
    let (pid, pname) = parsePrimIdAndName idAndName
        status' = getStatus status
        pairsCompleted' = parseDouble pairsCompleted
        pairsNum' = parseInt pairsNum
        pairsFailed' = parseDouble pairsFailed
    in Right $ JobInfo
        { jobId = pid
        , jobName = pname
        , jobStatus = status'
        , jobPairsCompleted = pairsCompleted'
        , jobPairsNum = pairsNum'
        , jobPairsFailed = pairsFailed'
        , jobDate = date
        }
getJobInfoFromTexts _ = Left "parse error in getJobInfoFromTexts"


getBenchmarkInfoFromTexts :: [Text] -> Either String BenchmarkInfo
getBenchmarkInfoFromTexts
  (idAndName:bType:rest) =
    let (pid, pname) = parsePrimIdAndName idAndName
    in Right $ BenchmarkInfo
        { benchmarkId = pid
        , benchmarkName = pname
        , benchmarkType = bType
        }
getBenchmarkInfoFromTexts _ = Left "parse error in getBenchmarkInfoFromTexts"

getUserInfoFromTexts :: [Text] -> Either String UserInfo
getUserInfoFromTexts
  (idAndName:institution:mail:rest) =
    let (pid, pname) = parsePrimIdAndName idAndName
        mail = parseMail mail
    in Right $ UserInfo
        { userId = pid
        , userName = pname
        , userInstitution = institution
        , userMail = mail
        }
getUserInfoFromTexts _ = Left "parse error in getUserInfoFromTexts"

getSpaceInfoFromTexts :: [Text] -> Either String SpaceInfo
getSpaceInfoFromTexts 
  (idAndName:desc:rest) = 
    let (pid, pname) = parsePrimIdAndName idAndName
    in Right $ SpaceInfo
        { spaceId = pid
        , spaceName = pname
        , spaceDescription = desc
        }
getSpaceInfoFromTexts _ = Left "parse error in getSpaceInfoFromTexts"

getSolverInfoFromTexts :: [Text] -> Either String SolverInfo
getSolverInfoFromTexts
  (idAndName:desc:rest) =
    let (pid, pname) = parsePrimIdAndName idAndName
    in Right $ SolverInfo
        { solverId = pid
        , solverName = pname
        , solverDescription = desc
        }
getSolverInfoFromTexts _ = Left "parse error in getSolverInfoFromTexts"