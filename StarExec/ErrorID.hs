module StarExec.ErrorID where

import Prelude
import Yesod

data ErrorID = Login | Unkown
    deriving (Eq, Show, Read)

{- instance ToMarkup ErrorID where
    ToMarkup = ToMarkup . show -}

instance PathPiece ErrorID where
    toPathPiece = toPathPiece . show
    fromPathPiece e = do
        err <- fromPathPiece e
        return $ read err
