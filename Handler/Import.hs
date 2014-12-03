module Handler.Import where

import Import

import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as L

import Yesod.Auth
import Yesod.Form.Bootstrap3
import Data.Conduit
import Data.Conduit.Binary
import Control.Monad.Trans.Resource
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar

import Presenter.Internal.Stringish
import qualified Importer.LRI as LRI

data SourceSelection = LRISelection | UIBKSelection
  deriving (Eq, Ord, Read, Show)

data UploadContent = UploadContent
  { source :: SourceSelection
  , file :: FileInfo
  }

uploadForm :: Form UploadContent
uploadForm = renderBootstrap3 BootstrapBasicForm $ UploadContent
  <$> areq (selectFieldList [("LRI"::Text, LRISelection), ("UIBK"::Text, UIBKSelection)]) "Source:" Nothing
  <*> fileAFormReq "Archive:"

getImportR :: Handler Html
getImportR = do
  maid <- maybeAuthId
  ((_, widget), enctype) <- runFormPost uploadForm
  let mUploadContent = Nothing :: Maybe UploadContent
      mFormError = Nothing :: Maybe Text
      mBytes = Nothing :: Maybe BSL.ByteString
  defaultLayout $(widgetFile "import")

listEntries :: Tar.Entries e -> [String]
listEntries = Tar.foldEntries addEntry [] (\_ -> [])
  where
    addEntry entry list = Tar.entryPath entry : list

getFileContents :: Tar.Entries e -> [(String, BSL.ByteString)]
getFileContents = Tar.foldEntries processEntry [] (\_ -> [])
  where
    processEntry entry es =
      case Tar.entryContent entry of
        Tar.NormalFile bs _ -> (Tar.entryPath entry, bs):es
        _                   -> es

getBytes :: UploadContent -> Handler BSL.ByteString
getBytes uc = do
  let fileBytes = fileSource $ file uc
  bytes <- runResourceT $ fileBytes $$ sinkLbs
  let entries = (Tar.read . GZip.decompress) bytes
      contents = getFileContents entries
      parsedContents = map (LRI.parse . snd) contents
  return $ L.foldl' mngParsedContent "" $ zip (map fst contents) parsedContents
    where
      mngParsedContent bs (filename, parsedContent) =
        case parsedContent of
          Right r -> bs +> fromString filename +> "\n" +> (fromString $ show r)
          Left e -> bs +> fromString filename +> "\nError: " +> (fromString $ show e) 

postImportR :: Handler Html
postImportR = do
  maid <- maybeAuthId
  ((result, widget), enctype) <- runFormPost uploadForm
  let mUploadContent = case result of
        FormSuccess r -> Just r
        _ -> Nothing
      mFormError = case result of
        FormSuccess _ -> Nothing
        FormMissing -> Just ("FormMissing" :: Text)
        FormFailure ts -> Just (mconcat ts)
  mBytes <- case mUploadContent of
    Just uc -> getBytes uc >>= return . Just
    _ -> return Nothing
  defaultLayout $(widgetFile "import")
