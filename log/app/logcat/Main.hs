{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}

module Main where

import qualified Control.Exception as E
import Control.Monad (forever)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (group, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Generics
import GHC.RTS.Events.Incremental
  ( Decoder (..),
    decodeEvents,
    readEvents,
    readHeader,
  )
import Network.Socket
  ( Family (AF_UNIX),
    SockAddr (SockAddrUnix),
    Socket,
    SocketType (Stream),
    close,
    connect,
    socket,
    withSocketsDo,
  )
import Network.Socket.ByteString (recv)
import System.IO (hFlush, stdout)
import Text.Pretty.Simple (pPrint)
import GHC.RTS.Events (Event (..), EventInfo (..))

histo :: [String] -> [(String, Int)]
histo = mapMaybe count . group . sort
  where
    count ys@(x:xs) = Just (x, length ys)
    count [] = Nothing

eventInfoToString :: EventInfo -> String
eventInfoToString info =
  case info of
    EventBlock {} -> "EventBlock"
    UnknownEvent {} -> "UnknownEvent"
    Startup {} -> "Startup"
    Shutdown {} -> "Shutdown"
    CreateThread {} -> "CreateThread"
    RunThread {} -> "RunThread"
    StopThread {} -> "StopThread"
    ThreadRunnable {} -> "ThreadRunnable"
    MigrateThread {} -> "MigrateThread"
    WakeupThread {} -> "WakeupThread"
    ThreadLabel {} -> "ThreadLabel"
    CreateSparkThread {} -> "CreateSparkThread"
    SparkCounters {} -> "SparkCounters"
    SparkCreate {} -> "SparkCreate"
    SparkDud {} -> "SparkDud"
    SparkOverflow {} -> "SparkOverflow"
    SparkRun {} -> "SparkRun"
    SparkSteal {} -> "SparkSteal"
    SparkFizzle {} -> "SparkFizzle"
    SparkGC {} -> "SparkGC"
    TaskCreate {} -> "TaskCreate"
    TaskMigrate {} -> "TaskMigrate"
    TaskDelete {} -> "TaskDelete"
    RequestSeqGC {} -> "RequestSeqGC"
    RequestParGC {} -> "RequestParGC"
    StartGC {} -> "StartGC"
    GCWork {} -> "GCWork"
    GCIdle {} -> "GCIdle"
    GCDone {} -> "GCDone"
    EndGC {} -> "EndGC"
    GlobalSyncGC {} -> "GlobalSyncGC"
    GCStatsGHC {} -> "GCStatsGHC"
    MemReturn {} -> "MemReturn"
    HeapAllocated {} -> "HeapAllocated"
    HeapSize {} -> "HeapSize"
    BlocksSize {} -> "BlocksSize"
    HeapLive {} -> "HeapLive"
    HeapInfoGHC {} -> "HeapInfoGHC"
    CapCreate {} -> "CapCreate"
    CapDelete {} -> "CapDelete"
    CapDisable {} -> "CapDisable"
    CapEnable {} -> "CapEnable"
    CapsetCreate {} -> "CapsetCreate"
    CapsetDelete {} -> "CapsetDelete"
    CapsetAssignCap {} -> "CapsetAssignCap"
    CapsetRemoveCap {} -> "CapsetRemoveCap"
    RtsIdentifier {} -> "RtsIdentifier"
    ProgramArgs {} -> "ProgramArgs"
    ProgramEnv {} -> "ProgramEnv"
    OsProcessPid {} -> "OsProcessPid"
    OsProcessParentPid {} -> "OsProcessParentPid"
    WallClockTime {} -> "WallClockTime"
    Message {} -> "Message"
    UserMessage {} -> "UserMessage"
    UserMarker {} -> "UserMarker"
    Version {} -> "Version"
    ProgramInvocation {} -> "ProgramInvocation"
    CreateMachine {} -> "CreateMachine"
    KillMachine {} -> "KillMachine"
    CreateProcess {} -> "CreateProcess"
    KillProcess {} -> "KillProcess"
    AssignThreadToProcess {} -> "AssignThreadToProcess"
    EdenStartReceive {} -> "EdenStartReceive"
    EdenEndReceive {} -> "EdenEndReceive"
    SendMessage {} -> "SendMessage"
    ReceiveMessage {} -> "ReceiveMessage"
    SendReceiveLocalMessage {} -> "SendReceiveLocalMessage"
    InternString {} -> "InternString"
    MerStartParConjunction {} -> "MerStartParConjunction"
    MerEndParConjunction {} -> "MerEndParConjunction"
    MerEndParConjunct {} -> "MerEndParConjunct"
    MerCreateSpark {} -> "MerCreateSpark"
    MerFutureCreate {} -> "MerFutureCreate"
    MerFutureWaitNosuspend {} -> "MerFutureWaitNosuspend"
    MerFutureWaitSuspended {} -> "MerFutureWaitSuspended"
    MerFutureSignal {} -> "MerFutureSignal"
    MerLookingForGlobalThread {} -> "MerLookingForGlobalThread"
    MerWorkStealing {} -> "MerWorkStealing"
    MerLookingForLocalSpark {} -> "MerLookingForLocalSpark"
    MerReleaseThread {} -> "MerReleaseThread"
    MerCapSleeping {} -> "MerCapSleeping"
    MerCallingMain {} -> "MerCallingMain"
    PerfName {} -> "PerfName"
    PerfCounter {} -> "PerfCounter"
    PerfTracepoint {} -> "PerfTracepoint"
    HeapProfBegin {} -> "HeapProfBegin"
    HeapProfCostCentre {} -> "HeapProfCostCentre"
    InfoTableProv {} -> "InfoTableProv"
    HeapProfSampleBegin {} -> "HeapProfSampleBegin"
    HeapProfSampleEnd {} -> "HeapProfSampleEnd"
    HeapBioProfSampleBegin {} -> "HeapBioProfSampleBegin"
    HeapProfSampleCostCentre {} -> "HeapBioProfSampleCostCentre"
    HeapProfSampleString {} -> "HeapProfSampleString"
    ProfSampleCostCentre {} -> "ProfSampleCostCentre"
    ProfBegin {} -> "ProfBegin"
    UserBinaryMessage {} -> "UserBinaryMessage"
    ConcMarkBegin {} -> "ConcMarkBegin"
    ConcMarkEnd {} -> "ConcMarkEnd"
    ConcSyncBegin {} -> "ConcSyncBegin"
    ConcSyncEnd {} -> "ConcSyncEnd"
    ConcSweepBegin {} -> "ConcSweepBegin"
    ConcSweepEnd {} -> "ConcSweepEnd"
    ConcUpdRemSetFlush {} -> "ConcUpdRemSetFlush"
    NonmovingHeapCensus {} -> "NonmovingHeapCensus"
    TickyCounterDef {} -> "TickyCounterDef"
    TickyCounterSample {} -> "TickyCounterSample"
    TickyBeginSample {} -> "TickyBeginSample"

dump :: Socket -> IO ()
dump sock = goHeader ""
  where
    goHeader bs0 = do
      bs1 <- recv sock 1024
      let bs = bs0 <> bs1
      let lbs = BL.fromStrict bs
      let e = readHeader lbs
      case e of
        Left err -> print err >> goHeader bs
        Right (hdr, lbs') -> do
          pPrint hdr
          let dec0 = decodeEvents hdr
          goEvents hdr dec0 (BL.toStrict lbs')

    go :: Decoder Event -> BS.ByteString -> IO (Maybe (Decoder Event), BS.ByteString)
    go dec !bytes = do
      case dec of
        Produce ev dec' -> do
          pPrint ev
          hFlush stdout
          go dec' bytes
        Consume k ->
          if BS.null bytes
            then pure (Just dec, "")
            else go (k bytes) ""
        Done bytes' ->
          pure (Nothing, bytes')
        Error bytes' e -> do
          pPrint e
          hFlush stdout
          -- reset if error happens.
          pure (Nothing, "")

    goEvents hdr dec !bytes = do
      (mdec', bytes') <- go dec bytes
      let dec' = fromMaybe (decodeEvents hdr) mdec'
      bytes'' <- recv sock 1024
      goEvents hdr dec' (bytes' <> bytes'')

main :: IO ()
main = do
  withSocketsDo $ do
    let file = "/tmp/eventlog.sock"
        open = do
          sock <- socket AF_UNIX Stream 0
          connect sock (SockAddrUnix file)
          pure sock
    E.bracket open close dump
    pure ()
