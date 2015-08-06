module Presenter.StarExec.Space where

import Presenter.Model.StarExec

import Text.HTML.DOM
import Text.HTML.TagSoup
import Text.XML.Cursor
import Codec.Compression.GZip
import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as BSL

import Prelude
import System.IO
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.List ( isSuffixOf )

getDefaultSpaceXML :: MonadIO m => FilePath -> m (Maybe Space)
getDefaultSpaceXML fp = do
    s <- liftIO $ BSL.readFile fp
    makeSpace s


-- | this is applied to the contents of a zipped XML space description file
-- as downloaded from starexec.
makeSpace :: MonadIO m => BSL.ByteString -> m (Maybe Space)
makeSpace bs = do
  let archive = Zip.toArchive bs
      xml_entries = filter ( \ e -> isSuffixOf ".xml" $ Zip.eRelativePath e ) 
                 $ Zip.zEntries archive 
  let spaces =  case xml_entries of
        [ e ] -> do
          let cursor = cursorFromDOM $ Zip.fromEntry e
              root = laxElement "tns:Spaces" cursor >>= child
              solver r = r >>= laxElement "Solver" >>= \ s -> return $ SolverInSpace
                  { soId = case attribute "id" s of
                       [i] -> read $ T.unpack i ; _ -> -1
                  , soName = case attribute "name" s of
                                 [ n ] -> n ; _ -> "noname"
                  }
              walk :: [ Cursor ] -> [ Space ]
              walk r = r >>= laxElement "Space" >>= \ s -> return
                     Space { spId = case attribute "id" s of
                                 [ i ] -> read $ T.unpack i ; _ -> -1
                           , spName = case attribute "name" s of
                                 [ n ] -> n ; _ -> "noname"
                           , benchmarks_with_names = child s
                                >>= laxElement "benchmark" >>= \ b ->
                                  (,) <$> ( read <$> T.unpack <$> attribute "id" b )
                                      <*> attribute "name" b
                           , solvers = child s >>= \ c -> solver [c]
                           , children = child s >>= \ c ->  walk [c]
                           }
          walk root
        _ -> []

  case spaces of
      [s] -> return $ Just s
      _ -> do
          liftIO $ putStrLn "====== no space ======"
          return Nothing

cursorFromDOM :: BSL.ByteString -> Cursor
cursorFromDOM = fromDocument . Text.HTML.DOM.parseLBS

