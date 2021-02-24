module Cardano.RTView.EKG
    ( EKGStores
    , isItEKGMetric
    , mkEKGStores
    , storeEKGMetrics
    ) where

import           Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVarIO)
import           Control.Monad (forM)
import           Control.Monad.STM (atomically)
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           System.Metrics (Store, createGauge, newStore, sampleAll)
import           System.Metrics.Gauge (Gauge, set)

import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.Configuration (RemoteAddrNamed (..))
import           Cardano.BM.Data.LogItem (LOContent (..), LOMeta (..), LogObject (..))

type NodeName = Text
type MetricName = Text
type Gauges = HashMap MetricName (Gauge, UTCTime)
-- We have to store gauges separately to be able to update them later.
type EKGStores = HashMap NodeName (Store, Gauges)

mkEKGStores
  :: [RemoteAddrNamed]
  -> IO EKGStores
mkEKGStores acceptors = do
  stores <- forM acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
    emptyStore <- newStore
    let gaugesForThisStore = HM.empty
    return (nameOfNode, (emptyStore, gaugesForThisStore))
  return $ HM.fromList stores

isItEKGMetric :: (Text, LogObject Text) -> Bool
isItEKGMetric (_, LogObject _ _ aContent) =
  case aContent of
    LogValue vName _ -> any (\s -> s `T.isInfixOf` vName) ekgMetricSigns
    _ -> False
 where
  ekgMetricSigns = ["Stat.", "Mem.", "Sys.", "Net.", "IO.", "RTS."]

storeEKGMetrics
  :: [(Text, LogObject Text)]
  -> TVar EKGStores
  -> IO ()
storeEKGMetrics [] _ = return ()
storeEKGMetrics metrics ekgStoresTVar = do
  updatedEKGStores <- doStoreEKGMetrics 0 metrics =<< readTVarIO ekgStoresTVar
  atomically $ modifyTVar' ekgStoresTVar (const updatedEKGStores)

doStoreEKGMetrics
  :: Int
  -> [(Text, LogObject Text)]
  -> EKGStores
  -> IO EKGStores
doStoreEKGMetrics ix metrics stores =
  if length metrics == ix
    then return stores
    else do
      let (loggerName, LogObject _ aMeta (LogValue metricName metricValue)) = metrics !! ix
          loggerNameParts = filter (not . T.null) $ T.splitOn "." loggerName
          nameOfNode = loggerNameParts !! 3
          rawValue :: Int64
          rawValue =
            case metricValue of
              Nanoseconds n  -> fromIntegral n
              Microseconds m -> fromIntegral m
              Bytes b        -> fromIntegral b
              PureI i        -> fromIntegral i
              _              -> 0
          (storeForThisNode, gaugesForThisStore) = stores ! nameOfNode
          ts = tstamp aMeta
      metricIsHere <- metricAlreadyStored metricName storeForThisNode
      updatedGauges <-
        if metricIsHere
          then do
            -- Since metric is already here, just update its value.
            updateGauge metricName gaugesForThisStore rawValue
            -- Update timestamp for this metric.
            return $ HM.adjust (\(g, _) -> (g, ts)) metricName gaugesForThisStore
          else do
            -- There is no such a metric, create it and set the value.
            newGauge <- createGauge metricName storeForThisNode
            set newGauge rawValue
            return $ HM.insert metricName (newGauge, ts) gaugesForThisStore

      let updatedStores = HM.adjust (const (storeForThisNode, updatedGauges)) nameOfNode stores
      doStoreEKGMetrics (ix + 1) metrics updatedStores

metricAlreadyStored
  :: Text
  -> Store
  -> IO Bool
metricAlreadyStored metricName store = HM.member metricName <$> sampleAll store

updateGauge
  :: Text
  -> Gauges
  -> Int64
  -> IO ()  
updateGauge metricName gauges rawValue = set gauge rawValue
 where 
  (gauge, _) = gauges ! metricName
