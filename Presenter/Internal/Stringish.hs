module Presenter.Internal.Stringish where

class Stringish a where
  (+>) :: a -> a -> a
