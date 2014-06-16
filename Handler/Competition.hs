module Handler.Competition where

import Import
import StarExec.Types

currentCompetition :: Text -> Competition
currentCompetition compName = Competition compName
  [ MetaCategory "Termination"
      [ Category "TRS_Standard"
          [ YES ]
          [ 2166, 2168 ]
      , Category "SRS_Standard"
          [ CERTIFIED ]
          [ 2166, 2168 ]
      ]
  , MetaCategory "Termination 2"
      [ Category "TRS_Standard 2"
          [ NO ]
          [ 2166, 2168 ]
      , Category "SRS_Standard 2"
          [ MAYBE ]
          [ 2166, 2168 ]
      ]
  ]

getCompetitionR :: Text -> Handler Html
getCompetitionR compName = redirect $
  CompetitionWithConfigR $ currentCompetition compName
