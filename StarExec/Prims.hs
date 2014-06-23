module StarExec.Prims
  ( listPrim
  , fromEither
  , eitherToJobInfos
  , eitherToSpaceInfos
  , eitherToSolverInfos
  , eitherToBenchmarkInfos
  -- , eitherToUserInfos
  -- , searchPrimInHierchy
  -- , searchPrim
  ) where

import Import
import Prelude
import Data.Char
import Data.Aeson
import Data.Either
import Data.Maybe
import Text.XML
import Text.XML.Cursor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import StarExec.Types
import StarExec.PersistTypes
import StarExec.Urls
import StarExec.Connection
import Network.HTTP.Conduit
import Network.HTTP.Types.Header

getPostData :: Int -> [(BS.ByteString, BS.ByteString)]
getPostData columns =
  [ ("sEcho", "1")
  , ("iColumns", BSC.pack $ show columns)
  , ("sColumns", "")
  , ("iDisplayStart", "0")
  , ("iDisplayLength", "2147483647")
  , ("iSortCol_0", "0")
  , ("sSearch", "")
  , ("sSortDir_0", "asc") ]

listPrim :: StarExecConnection -> Int -> StarExecListType -> Handler [Either String PrimInfo]
listPrim (sec, man, cookies) primID primType = do
  let columns = getColumns primType
      sType = map toLower $ show primType
      reqPath = getURL
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
  resp <- sendRequest (req, man, cookies)
  let jsonObj = eitherDecode
        $ responseBody resp :: (Either String ListPrimResult)
  case jsonObj of
    Right result -> do
      return $ parsePrimInfos primType $ aaData result
    Left msg -> do
      liftIO $ putStrLn msg
      return []

findPrim :: [PrimInfo] -> Int -> Maybe PrimInfo
findPrim primInfos _primId = List.find isPrim primInfos
    where isPrim prim = primInfoId prim == _primId

findInM :: (a -> Handler (Maybe b)) -> [a] -> Handler (Maybe b)
findInM _ [] = return Nothing
findInM _pred (x:xs) = do
    result <- _pred x
    case result of
        Nothing -> findInM _pred xs
        Just _ -> return result

--searchPrimInHierchy :: StarExecConnection -> Int -> Int -> StarExecListType -> Handler (Maybe PrimInfo)
--searchPrimInHierchy con _spaceId _primId primType = do
--    mPrimList <- listPrim con _spaceId primType
--    let primList = fromEither mPrimList
--        mPrim = findPrim primList _primId
--    liftIO $ putStrLn "### searchPrimInHierchy -> ###"
--    liftIO $ print $ show mPrimList
--    liftIO $ putStrLn "### <- searchPrimInHierchy ###"
--    case mPrim of
--        Just _ -> do
--          return mPrim
--        Nothing -> do
--            subSpaces <- listPrim con _spaceId Spaces
--            let spaceList = eitherToSpaceInfos subSpaces
--                searchPrimInHierchy' space =
--                    searchPrimInHierchy con
--                                        (spaceId space)
--                                        _primId
--                                        primType
--            findInM searchPrimInHierchy' spaceList

--searchPrim :: StarExecConnection -> Int -> StarExecListType -> Handler (Maybe PrimInfo)
--searchPrim con _primId primType =
--    searchPrimInHierchy con 1128 _primId primType

fromEither :: [Either String PrimInfo] -> [PrimInfo]
fromEither = rights

eitherToJobInfos :: [Either String PrimInfo] -> [JobInfo]
eitherToJobInfos = toJobInfos . fromEither

eitherToSpaceInfos :: [Either String PrimInfo] -> [SpaceInfo]
eitherToSpaceInfos = toSpaceInfos . fromEither

eitherToSolverInfos :: [Either String PrimInfo] -> [SolverInfo]
eitherToSolverInfos = toSolverInfos . fromEither

eitherToBenchmarkInfos :: [Either String PrimInfo] -> [BenchmarkInfo]
eitherToBenchmarkInfos = toBenchmarkInfos . fromEither

--eitherToUserInfos :: [Either String PrimInfo] -> [UserInfo]
--eitherToUserInfos = toUserInfos . fromEither

eitherInfo :: (Either String a) -> (a -> b) -> Either String b
eitherInfo (Right info) dataConst = Right $ dataConst info
eitherInfo (Left msg) _ = Left msg

parsePrimInfo :: StarExecListType -> [Text] -> Either String PrimInfo
parsePrimInfo Jobs       info = eitherInfo (getJobInfoFromTexts info) PrimJobInfo
--parsePrimInfo Spaces     info = eitherInfo (getSpaceInfoFromTexts info) PrimSpaceInfo
parsePrimInfo Benchmarks info = eitherInfo (getBenchmarkInfoFromTexts info) PrimBenchmarkInfo
--parsePrimInfo Users      info = eitherInfo (getUserInfoFromTexts info) PrimUserInfo
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

parseBenchmarkType :: Text -> Text
parseBenchmarkType = parseMail

getJobInfoFromTexts :: [Text] -> Either String JobInfo
getJobInfoFromTexts
  (idAndName:status:pairsCompleted:pairsNum:pairsFailed:date:_) =
    let (pid, pname) = parsePrimIdAndName idAndName
        status' = getStatus status
        --pairsCompleted' = parseDouble pairsCompleted
        --pairsNum' = parseInt pairsNum
        --pairsFailed' = parseDouble pairsFailed
    in Right $ JobInfo pid
                       pname
                       status'
                       date
getJobInfoFromTexts _ = Left "parse error in getJobInfoFromTexts"

getBenchmarkInfoFromTexts :: [Text] -> Either String BenchmarkInfo
getBenchmarkInfoFromTexts
  (idAndName:bType:_) =
    let (pid, pname) = parsePrimIdAndName idAndName
    in Right $ BenchmarkInfo pid
                             pname
                             (parseBenchmarkType bType)
getBenchmarkInfoFromTexts _ = Left "parse error in getBenchmarkInfoFromTexts"

--getUserInfoFromTexts :: [Text] -> Either String UserInfo
--getUserInfoFromTexts
--  (idAndName:institution:mail:_) =
--    let (pid, pname) = parsePrimIdAndName idAndName
--        pMail = parseMail mail
--    in Right $ UserInfo
--        { userId = pid
--        , userName = pname
--        , userInstitution = institution
--        , userMail = pMail
--        }
--getUserInfoFromTexts _ = Left "parse error in getUserInfoFromTexts"

--getSpaceInfoFromTexts :: [Text] -> Either String SpaceInfo
--getSpaceInfoFromTexts 
--  (idAndName:desc:_) = 
--    let (pid, pname) = parsePrimIdAndName idAndName
--    in Right $ SpaceInfo
--        { spaceId = pid
--        , spaceParentId = Nothing
--        , spaceName = pname
--        , spaceDescription = desc
--        }
--getSpaceInfoFromTexts _ = Left "parse error in getSpaceInfoFromTexts"

getSolverInfoFromTexts :: [Text] -> Either String SolverInfo
getSolverInfoFromTexts
  (idAndName:desc:_) =
    let (pid, pname) = parsePrimIdAndName idAndName
    in Right $ SolverInfo pid
                          pname
                          desc
getSolverInfoFromTexts _ = Left "parse error in getSolverInfoFromTexts"
