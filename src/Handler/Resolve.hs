module Handler.Resolve where

import Import
import Presenter.DOI

getResolveR :: DOI -> Handler Html
getResolveR doi = do
  serv <- doiService <$> getYesod
  render <- getUrlRender
  case toBench serv doi of
    Just ben -> redirect $ render $ ShowBenchmarkInfoR ben
    Nothing -> defaultLayout [whamlet|
<h1>#{show doi} does not refer to a benchmark
|]
