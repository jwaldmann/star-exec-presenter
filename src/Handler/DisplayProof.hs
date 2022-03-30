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

getDisplayProofR :: JobPairID -> Handler TypedContent
getDisplayProofR pairId@(StarExecPairID pid) = do
  mPersistJobPair <- runDB $ getBy $ UniqueJobPairInfo pid
  case mPersistJobPair of
    Nothing -> renderFail pairId "no such job pair"
    Just jp -> case jobPairInfoHtmlProof $ entityVal jp of
      Nothing -> renderFail pairId "job pair output has no html/xml contents"
      Just hp -> do
        return $ with_prologue cpfHTML $ decompress $ BSL.fromStrict hp

cpfHTML = Prologue
    { prologueBefore =
        [MiscInstruction
         (Instruction { instructionTarget = "xml-stylesheet"
                      , instructionData = "type=\"text/xsl\" href=\"/static/xsl/cpfHTML.xsl\""})
        ]
    , prologueDoctype = Nothing, prologueAfter = []
    }

with_prologue prolog proof = case Text.XML.parseLBS def proof of
  Right doc -> 
    let doc' = doc { documentPrologue = prolog }
    in  toTypedContent ( typeXml, toContent $ repXml $ renderLBS def doc' )
  Left exc -> 
    toTypedContent $ (typeHtml, toContent proof)



renderFail :: JobPairID -> Text -> Handler TypedContent
renderFail pid msg = do
          html <- defaultLayout $ do
                    [whamlet| 
                      <p>job pair #{show pid}: #{msg}
                    |]
          return $ toTypedContent $ (typeHtml, toContent html)

isHTML :: Cursor -> Bool
isHTML dom = let htmlRoot = element "html" dom
             in case htmlRoot of
              [] -> False
              _ -> True
