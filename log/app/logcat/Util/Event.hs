module Util.Event
  ( eventInfoToString,
    eventInfoEnumMap,
  )
where

import GHC.RTS.Events (EventInfo (..))

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

-- TODO: this should be read off right from RTS def.
eventInfoEnumMap :: [(String, Int)]
eventInfoEnumMap =
  [ ("EventBlock", 1),
    ("UnknownEvent", 2),
    ("Startup", 3),
    ("Shutdown", 4),
    ("CreateThread", 5),
    ("RunThread", 6),
    ("StopThread", 7),
    ("ThreadRunnable", 8),
    ("MigrateThread", 9),
    ("WakeupThread", 10),
    ("ThreadLabel", 11),
    ("CreateSparkThread", 12),
    ("SparkCounters", 13),
    ("SparkCreate", 14),
    ("SparkDud", 15),
    ("SparkOverflow", 16),
    ("SparkRun", 17),
    ("SparkSteal", 18),
    ("SparkFizzle", 19),
    ("SparkGC", 20),
    ("TaskCreate", 21),
    ("TaskMigrate", 22),
    ("TaskDelete", 23),
    ("RequestSeqGC", 24),
    ("RequestParGC", 25),
    ("StartGC", 26),
    ("GCWork", 27),
    ("GCIdle", 28),
    ("GCDone", 29),
    ("EndGC", 30),
    ("GlobalSyncGC", 31),
    ("GCStatsGHC", 32),
    ("MemReturn", 33),
    ("HeapAllocated", 34),
    ("HeapSize", 35),
    ("BlocksSize", 36),
    ("HeapLive", 37),
    ("HeapInfoGHC", 38),
    ("CapCreate", 39),
    ("CapDelete", 40),
    ("CapDisable", 41),
    ("CapEnable", 42),
    ("CapsetCreate", 43),
    ("CapsetDelete", 44),
    ("CapsetAssignCap", 45),
    ("CapsetRemoveCap", 46),
    ("RtsIdentifier", 47),
    ("ProgramArgs", 48),
    ("ProgramEnv", 49),
    ("OsProcessPid", 50),
    ("OsProcessParentPid", 51),
    ("WallClockTime", 52),
    ("Message", 53),
    ("UserMessage", 54),
    ("UserMarker", 55),
    ("Version", 56),
    ("ProgramInvocation", 57),
    ("CreateMachine", 58),
    ("KillMachine", 59),
    ("CreateProcess", 60),
    ("KillProcess", 61),
    ("AssignThreadToProcess", 62),
    ("EdenStartReceive", 63),
    ("EdenEndReceive", 64),
    ("SendMessage", 65),
    ("ReceiveMessage", 66),
    ("SendReceiveLocalMessage", 67),
    ("InternString", 68),
    ("MerStartParConjunction", 69),
    ("MerEndParConjunction", 70),
    ("MerEndParConjunct", 71),
    ("MerCreateSpark", 72),
    ("MerFutureCreate", 73),
    ("MerFutureWaitNosuspend", 74),
    ("MerFutureWaitSuspended", 75),
    ("MerFutureSignal", 76),
    ("MerLookingForGlobalThread", 77),
    ("MerWorkStealing", 78),
    ("MerLookingForLocalSpark", 79),
    ("MerReleaseThread", 80),
    ("MerCapSleeping", 81),
    ("MerCallingMain", 82),
    ("PerfName", 83),
    ("PerfCounter", 84),
    ("PerfTracepoint", 85),
    ("HeapProfBegin", 86),
    ("HeapProfCostCentre", 87),
    ("InfoTableProv", 88),
    ("HeapProfSampleBegin", 89),
    ("HeapProfSampleEnd", 90),
    ("HeapBioProfSampleBegin", 91),
    ("HeapProfSampleCostCentre", 92),
    ("HeapProfSampleString", 93),
    ("ProfSampleCostCentre", 94),
    ("ProfBegin", 95),
    ("UserBinaryMessage", 96),
    ("ConcMarkBegin", 97),
    ("ConcMarkEnd", 98),
    ("ConcSyncBegin", 99),
    ("ConcSyncEnd", 100),
    ("ConcSweepBegin", 101),
    ("ConcSweepEnd", 102),
    ("ConcUpdRemSetFlush", 103),
    ("NonmovingHeapCensus", 104),
    ("TickyCounterDef", 105),
    ("TickyCounterSample", 106),
    ("TickyBeginSample", 107)
  ]
