{-# language DoAndIfThenElse #-}

module Handler.InstallSolvers where

import Import
import Yesod.Auth

import Presenter.Registration (the_competition)
import Presenter.StarExec.Commands (addSolver)
import qualified Presenter.Registration.Code as C

import Data.Maybe
import Control.Monad ( forM_ )
import qualified Data.Text as T
import Control.Monad.Logger
import Data.List ( nub )

-- | link registered solvers to each space 
-- that contains benchmarks for category
-- where they are registered

getInstallSolversR :: Year -> Handler Html
getInstallSolversR year = do
  maid <- maybeAuthId
  if isJust maid then do
    let comp = the_competition year
    forM_ (C.metacategories comp) $ \ mc -> do
      logWarnN $ "installSolver.mc: " <> T.pack (show mc)
      forM_ (C.categories mc) $ \ cat -> do
        logWarnN $ "installSolver.cat " <> T.pack ( show cat)
        let solvers = nub $ do
              part <- C.participants $ C.contents cat
              solver@(sp,so,co) <- maybeToList $ C.solver_config part
              return (sp,so)
        let spaces = do
              C.Hierarchy sp <- C.benchmarks $ C.contents cat
              return sp
        forM_ (take 1 spaces) $ \ toSpace -> do
          forM_ (take 1 solvers) $ \ solver -> do
            addSolver toSpace [solver] True True 
            
    defaultLayout [whamlet|
                   <h1>Solvers copied for #{show year}
    |]
  else do
    defaultLayout [whamlet|
                   <h1>not authorized
    |]

  
