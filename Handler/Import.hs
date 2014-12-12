module Handler.Import where

import Import

import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as L
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL

import Yesod.Auth
import Yesod.Form.Bootstrap3
import Data.Either
import Data.Conduit
import Data.Conduit.Binary
import Control.Monad.Trans.Resource
import Control.Exception.Base
import qualified Codec.Archive.Zip as Zip
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
  <*> fileAFormReq "Zip-Archive:"

getImportR :: Handler Html
getImportR = do
  maid <- maybeAuthId
  ((_, widget), enctype) <- runFormPost uploadForm
  let mUploadContent = Nothing :: Maybe UploadContent
      mFormError = Nothing :: Maybe Text
  defaultLayout $(widgetFile "import")

readZip :: BSL.ByteString -> [(String, BSL.ByteString)]
readZip bs = zip (Zip.filesInArchive archive) (map Zip.fromEntry $ Zip.zEntries archive)
  where
    archive = Zip.toArchive bs

getBytes :: UploadContent -> Handler BSL.ByteString
getBytes uc = runResourceT $ (fileSource $ file uc) $$ sinkLbs

validateParsing :: Either String a -> Either String Bool
validateParsing p = do
  _ <- p
  return True

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p xs = case L.dropWhile p xs of
  [] -> []
  x -> w : splitBy p xs'
    where
      (w, xs') = break p x

handleError :: SomeException -> Handler ()
handleError e = lift $ do
  putStrLn "An error occurred while importing into the DB!"
  print e

normalizeString :: String -> String
normalizeString = map replaceString
  where
    replaceString '/' = '_'
    replaceString '\\' = '_'
    replaceString c = c

toText :: BSL.ByteString -> Text
toText = TL.toStrict . TLE.decodeUtf8

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
  case mUploadContent of
    Just uc -> case source uc of
      UIBKSelection -> return ()
      LRISelection -> forkHandler handleError $ importLRI uc
    _ -> return ()
  defaultLayout $(widgetFile "import")

importLRI :: UploadContent -> Handler ()
importLRI uc = do
  bytes <- getBytes uc
  let contents = readZip bytes
      entryNames = map (normalizeString . fst) contents
      parsedContents = map (LRI.parse . snd) contents
      results = map (>>= LRI.getResults) parsedContents
      solvers = map (>>= LRI.getSolvers) parsedContents
      benchmarks = map (>>= LRI.getBenchmarks) parsedContents
      parsings = concat
        [ map validateParsing parsedContents
        , map validateParsing results
        , map validateParsing solvers
        , map validateParsing benchmarks
        ]
  if all isRight parsings
    then runDB $ do
      clearLRI
      insertLRIResults $ zip entryNames $ rights results
      insertLRISolvers $ concat $ rights solvers
      insertLRIBenchmarks $ concat $ rights benchmarks
      insertLRIJobs $ entryNames
      return ()
    else do
      let invalids = filter isLeft parsings
      error $ "One or more errors occurred: " ++ (show invalids)

-- ### IMPORT LRI ###

clearLRI :: YesodDB App ()
clearLRI = do
  deleteWhere ([] :: [Filter LriResultInfo])
  deleteWhere ([] :: [Filter LriJobInfo])
  deleteWhere ([] :: [Filter LriSolverInfo])
  deleteWhere ([] :: [Filter LriBenchmarkInfo])
  return ()

insertLRIResults :: [(String, [LRI.LRIResult])] -> YesodDB App ()
insertLRIResults = Import.mapM_ insertMany
  where
    insertMany (jobName, rs) = Import.mapM_ (insert jobName) rs
    insert jobName r = insertUnique $ LriResultInfo
        (fromString jobName)
        Nothing
        (toText $ LRI.lrirPair r)
        (toText $ LRI.lrirBenchmark r)
        (toText $ LRI.lrirSolver r)
        (getResult $ LRI.lrirResult r)
        (LRI.lrirCpuTime r)
        (LRI.lrirWallclockTime r)
        (getResult <$> LRI.lrirCheckResult r)
        (LRI.lrirCheckCpuTime r)
        (LRI.lrirCheckWallclockTime r)
    getResult LRI.LRIYES    = YES Nothing
    getResult LRI.LRINO     = NO
    getResult LRI.LRIERROR  = ERROR
    getResult LRI.LRIMAYBE  = MAYBE
    getResult _             = OTHER

insertLRISolvers :: [LRI.LRISolver] -> YesodDB App ()
insertLRISolvers = Import.mapM_ insert
  where
    --insertMany (jobName, slvs) = Import.mapM_ (insert jobName) slvs
    insert s = insertUnique $ LriSolverInfo
        (toText $ LRI.lrisIdentifier s)
        (toText $ LRI.lrisName s)
        (toText $ LRI.lrisAuthor s)
        (toText $ LRI.lrisDescription s)
        (toText $ LRI.lrisURL s)
        (LRI.lrisStandard s)
        (LRI.lrisRelative s)
        (LRI.lrisConditional s)
        (LRI.lrisContextSensitive s)
        (LRI.lrisInnermost s)
        (LRI.lrisTheory s)
        (LRI.lrisCertifying s)

insertLRIBenchmarks :: [LRI.LRIBenchmark] -> YesodDB App ()
insertLRIBenchmarks = Import.mapM_ insert
  where
    --insertMany (jobName, bs) = Import.mapM_ (insert jobName) bs
    insert b = insertUnique $ LriBenchmarkInfo
      (toText $ LRI.lribIdentifier b)
      (toText $ LRI.lribName b)
      (toText $ LRI.lribFile b)
      (LRI.lribRating b)
      (LRI.lribSolved b)
      (LRI.lribConditional b)
      (LRI.lribContextSensitive b)
      (LRI.lribInnermost b)
      (LRI.lribOutermost b)
      (LRI.lribRelative b)
      (LRI.lribTheory b)

insertLRIJobs :: [String] -> YesodDB App ()
insertLRIJobs = Import.mapM_ $ \j -> do
  let tj = fromString j
  insertUnique $ LriJobInfo tj tj
