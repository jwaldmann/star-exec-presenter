module Presenter.Registration

( module Presenter.Registration.Code
, the_competition, extract
)

where

import Presenter.Registration.Code
import Presenter.Registration.Data
import qualified Presenter.Registration.Form_2016 as Form_2016
import qualified Presenter.Registration.Form_2015 as Form_2015
import Presenter.Model (Name, Year (..) )
import Prelude (($), fmap, (==))

the_competition :: Year -> Competition Catinfo
the_competition year = case year of
  Y2016 -> Form_2016.tc
  Y2015 -> Form_2015.tc
  E     -> Presenter.Registration.Data.experiment2015
  Y2014 -> Presenter.Registration.Data.tc2014

extract :: Year -> Name -> Competition [Participant]
extract year name = prune
           $ fmap participants
           $ filterP ( \p -> name == participantName p )
           $ the_competition year
