module Handler.CompetitionText where

import Import
import Yesod.Auth
import Data.Maybe
import qualified Data.Text as T
import Presenter.Output
import Data.String

-- inputForm :: Maybe Text -> MForm Handler (FormResult Textarea, Widget)
inputForm mc =
  let fs s = FieldSettings
        { fsLabel = fromString s
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [ ("cols", "80"), ("rows", "20") ]
        }
      compField = checkMMap (validateRead . unTextarea)
                            (Textarea . T.pack . show . output) textareaField
      utcField = checkMMap (validateRead) (T.pack . show) textField            
  in renderTable $ CompetitionInfo
       <$> areq compField (fs "competition") ( competitionInfoCompetition <$> mc )
       <*> areq utcField "date" ( competitionInfoDate <$> mc)
       <*> areq checkBoxField "public" ( competitionInfoPublic <$> mc )

validateRead t = return $ case readsPrec 0 $ T.unpack t of
        [(comp,"")] -> Right comp
        _ -> Left ( "parse error" :: T.Text )

getCompetitionTextR :: CompetitionInfoId -> Handler Html
getCompetitionTextR compId = do
  Just aid <- maybeAuthId
  Just cinfo <- runDB $ get compId
  (widget, enctype) <- generateFormPost $ inputForm $ Just cinfo
  defaultLayout [whamlet|
      <h1>Update CompetitionInfo #{show compId}
      <form method=post action=@{CompetitionTextR compId} entype=#{enctype}>
         <table> 
           ^{widget}
         <p>
           <button>Submit
|]

postCompetitionTextR :: CompetitionInfoId -> Handler Html
postCompetitionTextR compId = do
  Just aid <- maybeAuthId
  ((result, widget), enctype) <- runFormPost $ inputForm Nothing
  case result of
    FormSuccess t -> do
      runDB $ replace compId t
      defaultLayout [whamlet|<pre>updated: #{show $ output t} |]
