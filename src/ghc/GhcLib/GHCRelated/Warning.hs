{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

module GhcLib.GHCRelated.Warning (Warning, writeWarnings) where

import Data.IORef (IORef, modifyIORef)
import GHC qualified
import GHC.Driver.Session qualified as GHC
import GHC.Types.Error qualified as GHC
import GHC.Utils.Logger qualified as GHC

data Warning = GhcWarn
    { getMessageClass :: GHC.MessageClass
    , getSrcSpan      :: GHC.SrcSpan
    , getSDoc         :: GHC.SDoc
    }

instance Eq GHC.DiagnosticCode where
    GHC.DiagnosticCode n1 c1 == GHC.DiagnosticCode n2 c2 = n1 == n2 && c1 == c2

instance Eq GHC.MessageClass where
    GHC.MCOutput      == GHC.MCOutput      = True
    GHC.MCFatal       == GHC.MCFatal       = True
    GHC.MCInteractive == GHC.MCInteractive = True
    GHC.MCDump        == GHC.MCDump        = True
    GHC.MCInfo        == GHC.MCInfo        = True
    GHC.MCDiagnostic _ r1 mc1 == GHC.MCDiagnostic _ r2 mc2 = r1 == r2 && mc1 == mc2
    _                 == _             = False

uniqWarns :: Warning -> Warning -> Bool
-- | compare warnings based on getWarningReason and source location
uniqWarns w w' =
    getMessageClass w == getMessageClass w' && getSrcSpan w == getSrcSpan w'

writeWarnings
    :: IORef [Warning]
    -> (    GHC.LogFlags
         -> GHC.MessageClass
         -> GHC.SrcSpan
         -> GHC.SDoc
         -> IO ()
       )
    -> GHC.LogAction

-- | write warnings to IORef
writeWarnings ref _ = \flags msg_class srcSpan msg -> do
  modifyIORef ref (\xs -> GhcWarn msg_class srcSpan msg : xs)
  -- GHC.defaultLogAction flags msg_class srcSpan msg

-- replace noAction with defaultLogAction to output errors and warnings to stdout/stderr

getWarnLoc :: [Warning] -> [GHC.SrcSpan]

-- | Get location from warning
getWarnLoc = map getSrcSpan
