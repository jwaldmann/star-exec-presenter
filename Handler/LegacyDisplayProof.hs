module Handler.LegacyDisplayProof where

import Import
import Network.HTTP.Types.Status

getLegacyDisplayProofR :: JobPairID -> Handler TypedContent
getLegacyDisplayProofR = (redirectWith movedPermanently301) . DisplayProofR
