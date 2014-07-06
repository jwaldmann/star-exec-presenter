module Handler.DisplayHtmlProof where

import Import
import Text.Blaze
import Data.Text.Lazy.Encoding
import Codec.Compression.GZip
import qualified Data.ByteString.Lazy as BSL

getDisplayHtmlProofR :: Int -> Handler Html
getDisplayHtmlProofR _pairId = do
  mPersistJobPair <- runDB $ getBy $ UniqueJobPairInfo _pairId
  case mPersistJobPair of
    Nothing -> renderFail
    Just jp -> case jobPairInfoHtmlProof $ entityVal jp of
      Nothing -> renderFail
      Just hp -> return $ preEscapedToMarkup $ decodeUtf8 $ decompress $ BSL.fromStrict hp
  where renderFail = defaultLayout $ do
          [whamlet| 
            <p>Sorry, there is no job-pair with the id #{show _pairId} or it has no html-proof!
          |]
