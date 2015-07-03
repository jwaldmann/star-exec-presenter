module Presenter.Internal.Stringish where

import Prelude (String, Bool(..), (==))
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC

yes_ :: String
yes_ = "yes"

no_ :: String
no_ = "no"

notAvailable_ :: String
notAvailable_ = "not available"

class Monoid a => Stringish a where
  (+>) :: a -> a -> a
  (+>) = mappend
  toString :: a -> String
  fromString :: String -> a
  fromBool :: Bool -> a
  fromBool b = if b then yes else no
  toBool :: a -> Bool
  toBool a = toString a == yes_
  yes :: a
  yes = fromString yes_
  no :: a
  no = fromString no_
  notAvailable :: a
  notAvailable = fromString notAvailable_

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
