module Handler.CompetitionText where

import Import
import Yesod.Auth
import Data.Maybe
import qualified Data.Text as T
import Presenter.Output

-- inputForm :: Maybe Text -> MForm Handler (FormResult Textarea, Widget)
inputForm mcomp = renderDivs $ areq textareaField "input" ( fmap Textarea mcomp )

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
  ((result, widget), enctype) <- runFormPost $ inputForm Nothing
  case result of
    FormSuccess t -> do
      case readsPrec 0 $ T.unpack  $ unTextarea t :: [(Competition,String)] of
        [ (comp',"") ] -> do
          defaultLayout [whamlet|
            <p> #{show comp'}
            |]
