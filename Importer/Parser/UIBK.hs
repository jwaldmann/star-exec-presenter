module Importer.Parser.UIBK
  ( parse
  ) where

import Prelude
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as L
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Importer.Internal
import Data.Default
import Data.Monoid
import Debug.Trace

import Presenter.Internal.Stringish
import qualified Text.XML as XML
import qualified Text.XML.Cursor as Cursor

parseLBS :: BSL.ByteString -> Either String XML.Document
parseLBS bs =
  case XML.parseLBS def bs of
    Right doc -> Right doc
    Left e    -> Left $ show e

parseInt :: BSL.ByteString -> Int
parseInt bs = case reads $ toString bs of
  [(i,_)] -> i
  _       -> -1

toText :: BSL.ByteString -> Text
toText = TL.toStrict . TLE.decodeUtf8

fromText :: Text -> BSL.ByteString
fromText = TLE.encodeUtf8 . TL.fromStrict

el :: Text -> Cursor.Cursor -> [Cursor.Cursor]
el t c = Cursor.child c >>= Cursor.laxElement t

getContentFrom :: BSL.ByteString -> Cursor.Cursor -> BSL.ByteString
getContentFrom bs cursor =
  let t = toText bs
      contents = Cursor.child cursor >>=
                    Cursor.laxElement t >>=
                      Cursor.child >>=
                        Cursor.content
      c = mconcat contents
  in fromText c

getEntries :: Cursor.Cursor -> [UIBKResult]
getEntries c = el "normalEntry" c >>= \x -> do
  let resultID = parseInt $ getContentFrom "id" x
      problemID = parseInt $ getContentFrom "inputProblemID" x
      problemPath = getContentFrom "inputProblemPath" x
      result = getContentFrom "result" x
      wallcockTime = parseInt $ getContentFrom "wallclocktime" x
      tool = getContentFrom "tool" x
      toolVersion = getContentFrom "toolVersion" x
  return $ UIBKResult
    { uibkResultID = resultID
    , uibkResultInputProblemID = problemID
    , uibkResultInputProblemPath = problemPath
    , uibkResult = result
    , uibkResultWallclockTime = wallcockTime
    , uibkResultTool = tool
    , uibkResultToolVersion = toolVersion
    }


getCategories :: Cursor.Cursor -> [UIBKCategory]
getCategories c = Cursor.child c >>= Cursor.laxElement "category" >>= \x -> do
  let catID = parseInt $ getContentFrom "catID" x
      catName = getContentFrom "name" x
      entries = getEntries x
  return $ UIBKCategory
    { uibkCatID = catID
    , uibkCatName = catName
    , uibkCatEntries = entries
    }

getCompetitions :: [Cursor.Cursor] -> [UIBKCompetition]
getCompetitions cs = cs >>= Cursor.laxElement "competition" >>= \x -> do
  let compID = parseInt $ getContentFrom "compID" x
      compName = getContentFrom "name" x
      compCats = getCategories x
  return $ UIBKCompetition
    { uibkCompID = compID
    , uibkCompName = compName
    , uibkCompCategories = compCats
    }

parse :: BSL.ByteString -> Either String [UIBKCompetition]
parse bs = do
  doc <- parseLBS bs
  let cursor = Cursor.fromDocument doc
      root = Cursor.laxElement "competitions" cursor >>= Cursor.child
  return $ getCompetitions root
