module Handler.ListProofs where

import Import
import Presenter.PersistHelper
import Data.Maybe
import Presenter.Internal.Stringish

id2Text :: Pair -> Text
id2Text (StarExecPair p) = int2Text $ jobPairInfoPairId p

int2Text :: Int -> Text
int2Text = fromString . show

getPairID_ :: Pair -> JobPairID
getPairID_ (StarExecPair p) = StarExecPairID $ jobPairInfoPairId p

getResult_ :: Pair -> YesodDB App (Maybe JobResult)
getResult_ pair = do
  let _id = getPairID_ pair
  getPersistJobResult' _id

getJob_ :: ResultEntity a => Maybe a -> YesodDB App (Maybe Job)
getJob_ mResult = case mResult of
  Just result -> getPersistJobInfo' $ getJobID result
  Nothing -> return Nothing

getAll :: Handler [(Pair, Maybe JobResult, Maybe Job)]
getAll = runDB $ do
  starExecPairs <- do
    pairs <- getEntityList' [ JobPairInfoHtmlProof !=. Nothing ] []
    return $ StarExecPair <$> pairs -- (filter (isJust . jobPairInfoHtmlProof) pairs)
  starExecResults <- mapM getResult_ starExecPairs
  starExecJobs <- mapM getJob_ starExecResults
  return $ zip3 starExecPairs starExecResults starExecJobs

getListProofsR :: Handler Html
getListProofsR = do
  pairs <- getAll
  defaultLayout $ do
    $(widgetFile "list_proofs")
