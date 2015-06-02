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
import Presenter.Model.Competition (Competition)
import Prelude

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
