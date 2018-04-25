module Model where

import Yesod
import Data.Text (Text)
import Data.ByteString (ByteString)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Data.Time.Clock (UTCTime)
import Presenter.Model.StarExec
  ( JobResultStatus
  , SolverResult
  , JobStatus
  , SEQuery
  )
import Presenter.Model.Competition (Competition, Year)
import Presenter.DOI.Type (DOI)
import Prelude
import qualified Data.Text as T
import Control.Applicative

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


-- FIXME: this seems to be in the wrong place:

data CompetitionRef = CRefYear Year | CRefId CompetitionInfoId
  deriving (Eq, Ord, Show, Read)

instance PathPiece CompetitionRef where
  toPathPiece cr = case cr of
    CRefYear y -> toPathPiece y
    CRefId i -> toPathPiece i
  fromPathPiece t =
    (CRefYear <$> fromPathPiece t)
    <|> (CRefId <$> fromPathPiece t)

