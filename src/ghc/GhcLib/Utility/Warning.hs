{-# OPTIONS_GHC -Wno-orphans #-}

module GhcLib.Utility.Warning (Warning, writeWarnings) where

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
    GHC.SevFatal > s = True
    GHC.SevError > s = case s of
        GHC.SevFatal -> False
        _ -> True
    GHC.SevWarning > s = case s of
        GHC.SevFatal -> False
        GHC.SevError -> False
        _ -> True
    (<=) :: GHC.Severity -> GHC.Severity -> Bool
    _ <= GHC.SevFatal = True
    GHC.SevWarning <= GHC.SevError = True

uniqWarns :: Warning -> Warning -> Bool

-- | compare warnings based on getWarningReason and source location
uniqWarns w w' = getWarningReason w == getWarningReason w' && getWarningSpan w == getWarningSpan w'

writeWarnings :: IORef [Warning] -> GHC.LogAction -> GHC.LogAction

-- | write warnings to IORef
writeWarnings ref action dflags getWarningReason getWarningSeverity span getWarningDoc = do
    modifyIORef ref (GhcWarn getWarningReason getWarningSeverity span getWarningDoc :)
    noAction dflags getWarningReason getWarningSeverity span getWarningDoc

-- replace noAction with defaultLogAction to output errors and warnings to stdout/stderr

getWarnLoc :: [Warning] -> [GHC.SrcSpan]

-- | Get location from warning
getWarnLoc = map getWarningSpan

noAction :: GHC.LogAction
noAction _ _ _ _ _ = return ()