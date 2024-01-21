{-# LANGUAGE OverloadedLists #-}
module GhcLib.Utility.Flags (extFlags, unsetExtFlags, holeFlags, simplFlags, genFlags, unsetGenFlags, setWarnFlags, unsetWarnFlags) where

import GHC ()
import qualified GHC.Driver.Session as GHC 
import qualified GHC.LanguageExtensions.Type as GHC 


extFlags ::  [GHC.Extension]
-- | Extension flags to enable
extFlags =
    [ -- Default types are no longer limited to Num
      GHC.ExtendedDefaultRules
    ]

unsetExtFlags ::  [GHC.Extension]

-- | Extension flags to disable
unsetExtFlags =
    [ -- Type should be more concrete rather than general
      GHC.MonomorphismRestriction
    ]

holeFlags ::  [GHC.GeneralFlag]

-- | General flags concerning typed holes
holeFlags =
    [ -- show possible fits/suggestions in the warning message
      GHC.Opt_ShowValidHoleFits
    , -- Show constraints when reporting typed holes.
      GHC.Opt_ShowHoleConstraints
    , -- show the source location of the possible hole fits
      GHC.Opt_ShowProvOfHoleFits
    , GHC.Opt_ShowTypeAppVarsOfHoleFits
    , GHC.Opt_ShowTypeAppOfHoleFits
    , -- show the types of the possbile hole fits
      GHC.Opt_ShowTypeOfHoleFits
    , GHC.Opt_SortBySizeHoleFits
    , GHC.Opt_ShowMatchesOfHoleFits
    , GHC.Opt_UnclutterValidHoleFits
    ]

simplFlags ::  [GHC.GeneralFlag]

-- | Set flags for simplification pass
simplFlags =
    [ GHC.Opt_DoLambdaEtaExpansion
    , GHC.Opt_EnableRewriteRules
    ]

genFlags ::  [GHC.GeneralFlag]

-- | List of general flags to enable
genFlags =
    [ -- -dcore-lint
      -- ? not sure this is needed, as the link [https://downloads.haskell.org/ghc/latest/docs/users_guide/debugging.html#ghc-flag--dcore-lint], it checks the GH C and its runtime , not the user code
      GHC.Opt_DoCoreLinting
    , -- Convert typed hole errors into warnings, deferring the error until runtime.
      GHC.Opt_DeferTypedHoles
    , -- Turn type errors into warnings, deferring the error until runtime.
      GHC.Opt_DeferTypeErrors
    , -- defer the printing of error messages and warnings until the end of the compilation process and to group them by severity.
      GHC.Opt_DeferDiagnostics
    , -- GHC will build a mapping from info table pointers to source locations and some extra type information.
      -- track down memory leak using this : https://well-typed.com/blog/2021/01/first-look-at-hi-profiling-mode/
      GHC.Opt_InfoTableMap
    , -- automatically link in the base and rts packages.
      GHC.Opt_AutoLinkPackages
    ]

unsetGenFlags ::  [GHC.GeneralFlag]

-- | List of general flags to disable
unsetGenFlags =
    [ GHC.Opt_KeepHiFiles
    , GHC.Opt_KeepOFiles
    ]

setWarnFlags ::  [GHC.WarningFlag]

-- | List of warning flags to enable
setWarnFlags =
    [ -- pretty straight forward
      GHC.Opt_WarnOverlappingPatterns
    , GHC.Opt_WarnIncompletePatterns
    , -- must be used in conjunction with GHC.Opt_DeferTypedHoles
      GHC.Opt_WarnTypedHoles
    ]

unsetWarnFlags ::  [GHC.WarningFlag]

-- | List of warning flags to disable
unsetWarnFlags =
    [ GHC.Opt_WarnUnrecognisedPragmas
    , GHC.Opt_WarnInlineRuleShadowing
    ]