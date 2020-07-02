{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import Network.Connection
import Network.HTTP.Client.Conduit
import Network.HTTP.Conduit (mkManagerSettings)
import qualified Network.HTTP.Client.Conduit as NHCC
import Control.Monad.Logger (runLoggingT)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import Network.Wai.Logger (clockDateCacher)
import Data.Default (def)
import Data.Time.Clock (getCurrentTime)
import Yesod.Core.Types (loggerSet, Logger (Logger))

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.Control
import Handler.ListHiddenCompetitions
import Handler.ListCompetitions
import Handler.Registered
import Handler.ShowJobPair
import Handler.LegacyShowJobPair
import Handler.DisplayProof
import Handler.LegacyDisplayProof
import Handler.ShowJobInfo
import Handler.LegacyShowJobInfo
import Handler.ShowBenchmarkInfo
import Handler.LegacyShowBenchmarkInfo
import Handler.ShowSolverInfo
import Handler.LegacyShowSolverInfo
import Handler.ShowPostProcInfo
import Handler.LegacyShowPostProcInfo
import Handler.ShowManyJobResults
import Handler.LegacyShowManyJobResults
import Handler.LegacyShowJobResults
import Handler.FlexibleTable
import Handler.Competition
import Handler.CompetitionYear
import Handler.CompetitionWithConfig
import Handler.Import
import Handler.ListJobPairs
import Handler.ListProofs
import Handler.ListJobs
import Handler.ListBenchmarks
import Handler.RenderBenchmark
import Handler.ListSolvers
import Handler.ListPostProcs
import Handler.LegacyListCompetitions
import Handler.LegacyListHiddenCompetitions
import Handler.Problems
import Handler.ShowConfigInfo
import Handler.Participant
import Handler.Pause
import Handler.Resume
import Handler.Rerun
import Handler.InstallSolvers
import Handler.CompetitionText
import Handler.Resolve
import Handler.Concepts
import Handler.Combine

import qualified Data.Map.Strict as M
import Control.Concurrent.STM
import Presenter.StarExec.Connection (initial_login, LoginMethod(..))
import qualified Presenter.DOI as DOI

-- import Control.Concurrent.SSem
import qualified Control.Concurrent.FairRWLock as Lock

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO (Application, LogFunc)
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (appLogger foundation)
    return (logWare $ defaultMiddlewaresNoLogging app, logFunc)

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- NHCC.newManagerSettings $
      let -- disableCertificateValidation, see
          -- http://hackage.haskell.org/package/connection-0.2.3/docs/Network-Connection.html#t:TLSSettings
          tlsset = TLSSettingsSimple True False False
      in  ( mkManagerSettings tlsset Nothing )
          { managerResponseTimeout = responseTimeoutMicro $ 3600 * 10^6
          , managerConnCount = 10
          }
    cj <- initial_login Real manager
    -- Session for Connections to starexec.org
    now <- getCurrentTime
    session <- atomically $ newTVar
        $ SessionData cj Nothing now

    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher

    -- CompetitonResults-Cache
    crCache <- atomically $ newTVar M.empty
    -- DB-Semaphore
    dbS <- Lock.new

    -- Connection semaphore
    conS <- Lock.new

    -- resolver
    doiS <- DOI.makeResolverfrom
      -- you should only ever extend this list at the end,
      -- so that DOIs for old benchmarks remain stable.
      -- if DOIs change, you need to   "delete from job_result_info ;"
      [ "TPDB-65df8a308dd6_XML.zip"
      , "TPDB-10.3_XML.zip"
      , "johannes_waldmann_tpdb-8.0.7_XML.zip"
      , "TPDB-10.4_XML.zip"
      , "TPDB-10.5_XML.zip"
      , "mario_wenzel_XML.zip"
      , "TPDB-10.6a_XML.zip"
      , "TPDB-11_XML.zip"
      , "termcomp2019_XML.zip" -- contains new benchmarks uploaded by Akihisa, used in competition
      , "termcomp2020_XML.zip"
      ]

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App conf s p manager dbconf logger session crCache dbS conS doiS

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader (fmap fst . makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
