module StarExec.Concurrent where

import Import
import Yesod.Core.Handler
import Control.Exception.Base

type ExceptionHandler = SomeException -> Handler ()

baseExceptionHandler :: SomeException -> Handler ()
baseExceptionHandler e = do
  liftIO $ do
    putStrLn "There was an exception during a concurrent action:"
    putStrLn $ show e

runConcurrent :: ExceptionHandler -> Handler () -> Handler ()
runConcurrent errHandler action = forkHandler errHandler action

runBaseConcurrent :: Handler () -> Handler ()
runBaseConcurrent action = runConcurrent baseExceptionHandler action
