{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.RTView.ErrorBuffer
    ( ErrorBuffer
    , readErrorBuffer
    , effectuate
    , realize
    , unrealize
    ) where

import           Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Monad (when)
import           Control.Monad.STM (atomically)
import           Numeric.Natural (Natural)
import           Data.Aeson (FromJSON)
import qualified Data.Text.IO as TIO
import           System.IO (stderr)

import           Cardano.BM.Data.Backend (BackendKind (..), IsBackend (..), IsEffectuator (..))
import           Cardano.BM.Data.LogItem (LOContent (..), LOMeta (..), LogObject (..), LoggerName)
import           Cardano.BM.Data.Severity (Severity (..))

-- | All |LogObject|s accepted by |TraceAcceptor| plugin
--   will be decoded and traced to 'cardano-rt-view.acceptor'.
--   Because of RView configuration all these |LogObject|s
--   will be sent to |LogBufferBK| and |ErrorBufferBK|,
--   but |ErrorBufferBK| is storing only errors.
--   Later NodesState.Updater will extract errors from |ErrorBufferBK|
--   and display them in UI ("Errors" tab).
newtype ErrorBuffer a = ErrorBuffer
  { getErrBuf :: MVar (ErrorBufferInternal a)
  }

newtype ErrorBufferInternal a
  = ErrorBufferInternal
      { errQueue :: TBQ.TBQueue (LoggerName, LogObject a)
      }

-- | Once we read the current content of the queue, it should be cleaned.
readErrorBuffer :: ErrorBuffer a -> IO [(LoggerName, LogObject a)]
readErrorBuffer buffer =
  modifyMVar (getErrBuf buffer) $ \currentBuffer -> do
    loList <- atomically $ TBQ.flushTBQueue (errQueue currentBuffer)
    -- LogObjects are flushed, clean it up.
    queue <- atomically $ TBQ.newTBQueue queueMaxSize
    return (ErrorBufferInternal queue, loList)

instance IsEffectuator ErrorBuffer a where
  effectuate buffer lo@(LogObject loname lometa locontent) = do
    currentEB <- readMVar (getErrBuf buffer)
    let queue = errQueue currentEB
    noCapacity <- atomically $ TBQ.isFullTBQueue queue
    if noCapacity
      then handleOverflow buffer
      else when isError $
             atomically $ TBQ.writeTBQueue queue ("#buffered." <> loname, lo)
   where
    -- Only Error-messages should be stored in the queue.
    isError =
      case locontent of
        LogValue _ _ -> False
        LogError _   -> True
        _            -> severity lometa >= Error

  handleOverflow _ = TIO.hPutStrLn stderr "Notice: overflow in ErrorBuffer, dropping log items!"

instance FromJSON a => IsBackend ErrorBuffer a where
  bekind _ = UserDefinedBK "ErrorBufferBK"

  realize _ = do
    queue <- atomically $ TBQ.newTBQueue queueMaxSize
    ErrorBuffer <$> newMVar (ErrorBufferInternal queue)

  unrealize _ = return ()

queueMaxSize :: Natural
queueMaxSize = 1000
