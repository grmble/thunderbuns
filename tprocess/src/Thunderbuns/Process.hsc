{-# LANGUAGE CPP #-}

{- | Process helpers

* getPid: get the process id of the current process
-}
module Thunderbuns.Process
  ( Pid
  , getPid
  ) where
#ifdef WINDOWS
import System.Win32.Process (ProcessId, getCurrentProcessId)
#else
import System.Posix.Process (getProcessID)
import System.Posix.Types (ProcessID)
#endif

-- | Pid - alias for process id, system dependent
#ifdef WINDOWS
type Pid = ProcessId
#else
type Pid = ProcessID
#endif
-- | Get the process id of the current process
getPid :: IO Pid
#ifdef WINDOWS
getPid = getCurrentProcessId
#else
getPid = getProcessID
#endif
