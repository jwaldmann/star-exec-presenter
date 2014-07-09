module Handler.DisplayProof where

import Import
import Text.Blaze
import Data.Text.Lazy.Encoding
import Codec.Compression.GZip
import System.Directory
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TRead
import Text.HTML.DOM
import Text.XML.Cursor
import Debug.Trace

getDisplayProofR :: Text -> Handler Html
getDisplayProofR path = do
  case TRead.decimal path of
    Right (_pairId, _) -> getProof _pairId
    Left _ -> getFile path

getFile :: Text -> Handler Html
getFile path = do
  let filePath = "static/xsl/" ++ (T.unpack path)
  fileExists <- liftIO $ doesFileExist filePath
  if fileExists
    then do
      file <- liftIO $ TIO.readFile filePath
      addHeader "Content-Type" "text/xsl; charset=utf-8"
      return $ preEscapedToMarkup file
    else notFound

getProof :: Int -> Handler Html
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
          then return ()
          else addHeader "Content-Type" "text/xml; charset=utf-8"
        return $ preEscapedToMarkup $ decodeUtf8 $ proof
  where renderFail = defaultLayout $ do
          [whamlet| 
            <p>Sorry, there is no job-pair with the id #{show _pairId} or it has no html-proof!
          |]

isHTML :: Cursor -> Bool
isHTML dom = let htmlRoot = element "html" dom
             in case htmlRoot of
              [] -> False
              [e] -> True
             --trace (show htmlRoot) $ length htmlRoot > 0
