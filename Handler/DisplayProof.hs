module Handler.DisplayProof where

import Import
import Codec.Compression.GZip
import System.Directory
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TRead
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor
import Text.XML (Document(..), Instruction(..), Miscellaneous(..), Prologue(..)
                , parseLBS, renderLBS)
import Control.Monad.Logger
import Data.Default.Class (def)

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
      return $ toTypedContent (typeXSL, toContent file)
    else notFound

getProof :: Int -> Handler TypedContent
getProof _pairId = do
  mPersistJobPair <- runDB $ getBy $ UniqueJobPairInfo _pairId
  case mPersistJobPair of
    Nothing -> renderFail _pairId "no such job pair"
    Just jp -> case jobPairInfoHtmlProof $ entityVal jp of
      Nothing -> renderFail _pairId "job pair output has no html/xml contents"
      Just hp -> do
        let proof = decompress $ BSL.fromStrict hp
        case  Text.XML.parseLBS def proof of
          Right xml_doc -> do
            logWarnN $ T.pack $ "------------" ++ show ( documentPrologue xml_doc )
            let xml_doc' = repair_stylesheet xml_doc
            return $ toTypedContent ( typeXml, toContent $ repXml $ renderLBS def xml_doc )
          Left exc -> do
            let html_doc = Text.HTML.DOM.parseLBS proof
                cursor = fromDocument html_doc
            if isHTML cursor
              then return $ toTypedContent $ (typeHtml, toContent proof)
              else renderFail _pairId "job pair output has invalid html contents"

cpfHTML_prologue = Prologue
    { prologueBefore =
        [MiscInstruction
         (Instruction { instructionTarget = "xml-stylesheet"
                      , instructionData = "type=\"text/xsl\" href=\"cpfHTML.xsl\""})
        ]
    , prologueDoctype = Nothing, prologueAfter = []
    }

repair_stylesheet doc = doc { documentPrologue = cpfHTML_prologue }

renderFail :: Int -> Text -> Handler TypedContent
renderFail _pairId msg = do
          html <- defaultLayout $ do
                    [whamlet| 
                      <p>job pair #{show _pairId}: #{msg}
                    |]
          return $ toTypedContent $ (typeHtml, toContent html)

isHTML :: Cursor -> Bool
isHTML dom = let htmlRoot = element "html" dom
             in case htmlRoot of
              [] -> False
              _ -> True
