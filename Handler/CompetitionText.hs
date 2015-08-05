module Handler.CompetitionText where

import Import
import Yesod.Auth
import Data.Maybe
import qualified Data.Text as T
import Presenter.Output
import Data.String

-- inputForm :: Maybe Text -> MForm Handler (FormResult Textarea, Widget)
inputForm mcomp =
  let fs s = FieldSettings
        { fsLabel = fromString s
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [ ("cols", "80"), ("rows", "20") ]
        } 
  in renderDivs $ areq textareaField (fs "input") ( fmap Textarea mcomp )

getCompetitionTextR :: CompetitionInfoId -> Handler Html
getCompetitionTextR compId = do
  Just aid <- maybeAuthId
  Just cinfo <- runDB $ get compId
  let comp = competitionInfoCompetition cinfo
  (widget, enctype) <- generateFormPost $ inputForm $ Just $ T.pack $ show $ output comp
  defaultLayout [whamlet|
      <h1>Update CompetitionInfo #{show compId}
      <form method=post action=@{CompetitionTextR compId} entype=#{enctype}>
         ^{widget}
         <p>
           <button>Submit
|]

postCompetitionTextR :: CompetitionInfoId -> Handler Html
postCompetitionTextR compId = do
  Just aid <- maybeAuthId
  Just cinfo <- runDB $ get compId
  ((result, widget), enctype) <- runFormPost $ inputForm Nothing
  case result of
    FormSuccess t -> do
      case readsPrec 0 $ T.unpack  $ unTextarea t :: [(Competition,String)] of
        [ (comp',"") ] -> do
          runDB $ replace compId $ cinfo { competitionInfoCompetition = comp' }
          defaultLayout [whamlet|
            <p> #{show comp'}
            |]
