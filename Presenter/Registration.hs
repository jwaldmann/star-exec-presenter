module Presenter.Registration

( module Presenter.Registration.Code
, the_competition, extract
)

where

import Presenter.Registration.Code
import Presenter.Registration.Data
import Presenter.Registration.Form
import Presenter.Model (Name, Year (..) )
import Prelude (($), fmap, (==))

the_competition :: Year -> Competition Catinfo
the_competition year = case year of
  Y2015 -> Presenter.Registration.Form.tc_Y2015
  E     -> Presenter.Registration.Data.experiment2015
  Y2014 -> Presenter.Registration.Data.tc2014

extract :: Year -> Name -> Competition [Participant]
extract year name = prune
           $ fmap participants
           $ filterP ( \p -> name == participantName p )
           $ the_competition year
