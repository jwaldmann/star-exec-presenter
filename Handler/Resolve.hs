module Handler.Resolve where

import Import
import qualified Data.Map.Strict as M

getResolveR :: DOI -> Handler Html
getResolveR doi = do
  (to,from) <- doiService <$> getYesod
  let sol = M.lookup doi from
  defaultLayout [whamlet|
<h1>resolve #{show doi}
<p>name is #{show sol}
|]                 
