module Handler.LegacyDisplayProof where

import Import
import Handler.DisplayProof

getLegacyDisplayProofR :: Text -> Handler Html
getLegacyDisplayProofR = getDisplayProofR
