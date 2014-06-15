module Handler.Competition where

import Import
import StarExec.Types

currentCompetition :: Text -> Competition
currentCompetition compName = Competition compName
  [ MetaCategory "Termination"
      [ Category "TRS_Standard"
          [ YES ]
          [ 1095, 1217 ]
      , Category "SRS_Standard"
          [ CERTIFIED ]
          [ 1095, 1217 ]
      ]
  , MetaCategory "Termination 2"
      [ Category "TRS_Standard 2"
          [ NO ]
          [ 1095, 1217 ]
      , Category "SRS_Standard 2"
          [ MAYBE ]
          [ 1095, 1217 ]
      ]
  ]

getCompetitionR :: Text -> Handler Html
getCompetitionR compName = redirect $
  CompetitionWithConfigR $ currentCompetition compName
