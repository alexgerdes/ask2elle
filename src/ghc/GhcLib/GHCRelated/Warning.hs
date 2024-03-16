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
    { getWarningReason :: GHC.WarnReason
    , getWarningSeverity :: GHC.Severity
    , getWarningSpan :: GHC.SrcSpan
    , getWarningDoc :: GHC.SDoc
    }
    deriving stock (Show)

instance Eq GHC.WarnReason where
    (==) :: GHC.WarnReason -> GHC.WarnReason -> Bool
    (GHC.Reason flag) == (GHC.Reason flag') = flag == flag'
    (GHC.ErrReason f) == (GHC.ErrReason f') = f == f'
    GHC.NoReason == GHC.NoReason = True
    _ == _ = False

instance Eq Warning where
    -- \| Equality of warnings based on warning getWarningReason
    (==) :: Warning -> Warning -> Bool
    w == w' = getWarningReason w == getWarningReason w'

instance Ord Warning where
    (>) :: Warning -> Warning -> Bool
    w > w' = getWarningSeverity w > getWarningSeverity w'
    (<=) :: Warning -> Warning -> Bool
    w <= w' = getWarningSeverity w <= getWarningSeverity w'

instance Ord GHC.Severity where
    (>) :: GHC.Severity -> GHC.Severity -> Bool
    GHC.SevFatal > _ = True
    GHC.SevError > GHC.SevError = False
    GHC.SevError > _ = True
    GHC.SevWarning > GHC.SevFatal = False
    GHC.SevWarning > GHC.SevError = False
    GHC.SevWarning > _ = True
    GHC.SevInteractive > GHC.SevFatal = False
    GHC.SevInteractive > GHC.SevError = False
    GHC.SevInteractive > GHC.SevWarning = False
    GHC.SevInteractive > _ = True
    GHC.SevOutput > GHC.SevFatal = False
    GHC.SevOutput > GHC.SevError = False
    GHC.SevOutput > GHC.SevWarning = False
    GHC.SevOutput > GHC.SevInteractive = False
    GHC.SevOutput > _ = True
    GHC.SevDump > GHC.SevFatal = False
    GHC.SevDump > GHC.SevError = False
    GHC.SevDump > GHC.SevWarning = False
    GHC.SevDump > GHC.SevInteractive = False
    GHC.SevDump > GHC.SevOutput = False
    GHC.SevDump > _ = True
    GHC.SevInfo > GHC.SevFatal = False
    GHC.SevInfo > GHC.SevError = False
    GHC.SevInfo > GHC.SevWarning = False
    GHC.SevInfo > GHC.SevInteractive = False
    GHC.SevInfo > GHC.SevOutput = False
    GHC.SevInfo > GHC.SevDump = False
    GHC.SevInfo > _ = True
    (<=) :: GHC.Severity -> GHC.Severity -> Bool
    GHC.SevFatal <= GHC.SevFatal = True
    GHC.SevFatal <= _ = False
    GHC.SevError <= GHC.SevFatal = True
    GHC.SevError <= GHC.SevError = True
    GHC.SevError <= _ = False
    GHC.SevWarning <= GHC.SevFatal = True
    GHC.SevWarning <= GHC.SevError = True
    GHC.SevWarning <= GHC.SevWarning = True
    GHC.SevWarning <= _ = False
    GHC.SevInteractive <= GHC.SevFatal = True
    GHC.SevInteractive <= GHC.SevError = True
    GHC.SevInteractive <= GHC.SevWarning = True
    GHC.SevInteractive <= GHC.SevInteractive = True
    GHC.SevInteractive <= _ = False
    GHC.SevOutput <= GHC.SevFatal = True
    GHC.SevOutput <= GHC.SevError = True
    GHC.SevOutput <= GHC.SevWarning = True
    GHC.SevOutput <= GHC.SevInteractive = True
    GHC.SevOutput <= GHC.SevOutput = True
    GHC.SevOutput <= _ = False
    GHC.SevDump <= GHC.SevFatal = True
    GHC.SevDump <= GHC.SevError = True
    GHC.SevDump <= GHC.SevWarning = True
    GHC.SevDump <= GHC.SevInteractive = True
    GHC.SevDump <= GHC.SevOutput = True
    GHC.SevDump <= GHC.SevDump = True
    GHC.SevDump <= _ = False
    GHC.SevInfo <= _ = True

uniqWarns :: Warning -> Warning -> Bool

-- | compare warnings based on getWarningReason and source location
uniqWarns w w' = getWarningReason w == getWarningReason w' && getWarningSpan w == getWarningSpan w'

writeWarnings
    :: IORef [Warning]
    -> ( GHC.DynFlags
         -> GHC.WarnReason
         -> GHC.Severity
         -> GHC.SrcSpan
         -> GHC.SDoc
         -> IO ()
       )
    -> GHC.LogAction

-- | write warnings to IORef
writeWarnings ref _ =
    \dflags getWarningReason getWarningSeverity warningSpan getWarningDoc -> do
        modifyIORef ref (\xs -> GhcWarn getWarningReason getWarningSeverity warningSpan getWarningDoc : xs)
        noAction dflags getWarningReason getWarningSeverity warningSpan getWarningDoc

-- replace noAction with defaultLogAction to output errors and warnings to stdout/stderr

getWarnLoc :: [Warning] -> [GHC.SrcSpan]

-- | Get location from warning
getWarnLoc = map getWarningSpan

noAction :: GHC.LogAction
noAction _ _ _ _ _ = return ()
