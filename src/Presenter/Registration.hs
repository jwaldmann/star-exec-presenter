{-# language OverloadedStrings #-}

module Presenter.Registration

( module Presenter.Registration.Code
, the_competition, extract
, catcat, Catcat (..)
)

where

import Presenter.Registration.Code
import Presenter.Registration.Data
import qualified Presenter.Registration.Form_2018 as Form_2018
import qualified Presenter.Registration.Form_2017 as Form_2017
import qualified Presenter.Registration.Form_2016 as Form_2016
import qualified Presenter.Registration.Form_2015 as Form_2015
import Presenter.Model (Name, Year (..) )
import Prelude (($), fmap, (==), elem, Show)

the_competition :: Year -> Competition Catinfo
the_competition year = case year of
  Y2018 -> Form_2018.tc
  Y2017 -> Form_2017.tc
  Y2016 -> Form_2016.tc
  Y2015 -> Form_2015.tc
  E     -> Presenter.Registration.Data.experiment2015
  Y2014 -> Presenter.Registration.Data.tc2014

extract :: Year -> Name -> Competition [Participant]
extract year name = prune
           $ fmap participants
           $ filterP ( \p -> name == participantName p )
           $ the_competition year

data Catcat = Rewriting | Programs deriving (Show)

catcat :: Category a -> Catcat
catcat c =
  if categoryName c `elem`
     [ "C", "C Integer Programs", "Java Bytecode", "Logic Programming", "Functional Programming"
     , "Complexity - C Integer Programs"
     ]
  then Programs else Rewriting
                    

    
