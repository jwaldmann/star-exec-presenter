module Presenter.Internal.Stringish where

import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

class Monoid a => Stringish a where
  (+>) :: a -> a -> a
  (+>) = mappend

instance Stringish T.Text where
  (+>) = T.append

instance Stringish TL.Text where
  (+>) = TL.append

instance Stringish BS.ByteString where
  (+>) = BS.append

instance Stringish BSL.ByteString where
  (+>) = BSL.append
