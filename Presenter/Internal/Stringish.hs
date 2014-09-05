module Presenter.Internal.Stringish where

import Prelude (String)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC

class Monoid a => Stringish a where
  (+>) :: a -> a -> a
  (+>) = mappend
  toString :: a -> String
  fromString :: String -> a

instance Stringish T.Text where
  (+>) = T.append
  toString = T.unpack
  fromString = T.pack

instance Stringish TL.Text where
  (+>) = TL.append
  toString = TL.unpack
  fromString = TL.pack

instance Stringish BS.ByteString where
  (+>) = BS.append
  toString = BSC.unpack
  fromString = BSC.pack

instance Stringish BSL.ByteString where
  (+>) = BSL.append
  toString = BSLC.unpack
  fromString = BSLC.pack
