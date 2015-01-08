module Handler.LegacyDisplayProof where

import Import
import Network.HTTP.Types.Status

getLegacyDisplayProofR :: Text -> Handler TypedContent
getLegacyDisplayProofR = (redirectWith movedPermanently301) . DisplayProofR
