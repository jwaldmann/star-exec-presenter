-- | this the DOI (digital object identifier) idea.
-- specifically, we are talking about TPI
-- (termination problem identifier)

module Presenter.DOI.Type

( DOI
, makeTPI
)
       
where
  
import Prelude
import Yesod
import qualified Data.Text as T

data DOI = TPI Int
  deriving (Eq, Ord)

instance Show DOI where
  show (TPI i) = "tpi:" ++ show i

instance Read DOI where
  readsPrec p ('t':'p':'i':':':s) = do
    (n,rest) <- readsPrec p s
    return (TPI n, rest)

makeTPI = TPI

$(derivePersistField "DOI")

instance PathPiece DOI where
  toPathPiece = T.pack . show
  fromPathPiece t =
    case readsPrec 0 $ T.unpack t of
      [(d,"")] -> return d
      _ -> Nothing
      
