module Handler.DisplayProof where

import Import
import Codec.Compression.GZip
import System.Directory
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TRead
import Text.HTML.DOM
import Text.XML.Cursor

typeXSL :: ContentType
typeXSL = "text/xsl"

getDisplayProofR :: Text -> Handler TypedContent
getDisplayProofR path = do
  case TRead.decimal path of
    Right (_pairId, _) -> getProof _pairId
    Left _ -> getFile path

getFile :: Text -> Handler TypedContent
getFile path = do
  let filePath = "static/xsl/" ++ (T.unpack path)
  fileExists <- liftIO $ doesFileExist filePath
  if fileExists
    then do
      file <- liftIO $ TIO.readFile filePath
      --addHeader "Content-Type" "text/xsl; charset=utf-8"
      return $ toTypedContent (typeXSL, toContent file)
    else notFound

getProof :: Int -> Handler TypedContent
getProof _pairId = do
  mPersistJobPair <- runDB $ getBy $ UniqueJobPairInfo _pairId
  case mPersistJobPair of
    Nothing -> renderFail
    Just jp -> case jobPairInfoHtmlProof $ entityVal jp of
      Nothing -> renderFail
      Just hp -> do
        let proof = decompress $ BSL.fromStrict hp
            dom = fromDocument $ parseLBS proof
        if isHTML dom
          then do
            liftIO $ putStrLn "document is html"
            return $ toTypedContent $ (typeHtml, toContent proof)
          else do
            liftIO $ putStrLn "document is xml"
            return $ toTypedContent $ repXml proof
            --addHeader "Content-Type" "text/xml; charset=utf-8"
        --return $ preEscapedToMarkup $ decodeUtf8 $ proof
  where renderFail = do
          html <- defaultLayout $ do
                    [whamlet| 
                      <p>Sorry, there is no job-pair with the id #{show _pairId} or it has no html-proof!
                    |]
          return $ toTypedContent $ (typeHtml, toContent html)

isHTML :: Cursor -> Bool
isHTML dom = let htmlRoot = element "html" dom
             in case htmlRoot of
              [] -> False
              _ -> True
             --trace (show htmlRoot) $ length htmlRoot > 0
