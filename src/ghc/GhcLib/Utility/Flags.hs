module GhcLib.Utility.Flags (extFlags, unsetExtFlags, holeFlags, simplFlags, genFlags, unsetGenFlags, setWarnFlags, unsetWarnFlags) where

import GHC ()
import GHC.Driver.Session (GeneralFlag (..), WarningFlag (..))
import GHC.LanguageExtensions.Type (Extension (..))

extFlags :: [Extension]

-- | Extension flags to enable
extFlags =
    [ -- Default types are no longer limited to Num
      ExtendedDefaultRules
    ]

unsetExtFlags :: [Extension]

-- | Extension flags to disable
unsetExtFlags =
    [ -- Type should be more concrete rather than general
      MonomorphismRestriction
    ]

holeFlags :: [GeneralFlag]

-- | General flags concerning typed holes
holeFlags =
    [ -- show possible fits/suggestions in the warning message
      Opt_ShowValidHoleFits
    , -- Show constraints when reporting typed holes.
      Opt_ShowHoleConstraints
    , -- show the source location of the possible hole fits
      Opt_ShowProvOfHoleFits
    , Opt_ShowTypeAppVarsOfHoleFits
    , Opt_ShowTypeAppOfHoleFits
    , -- show the types of the possbile hole fits
      Opt_ShowTypeOfHoleFits
    , Opt_SortBySizeHoleFits
    , Opt_ShowMatchesOfHoleFits
    , Opt_UnclutterValidHoleFits
    ]

simplFlags :: [GeneralFlag]

-- | Set flags for simplification pass
simplFlags =
    [ Opt_DoLambdaEtaExpansion
    , Opt_EnableRewriteRules
    ]

genFlags :: [GeneralFlag]

-- | List of general flags to enable
genFlags =
    [ -- -dcore-lint
      -- ? not sure this is needed, as the link [https://downloads.haskell.org/ghc/latest/docs/users_guide/debugging.html#ghc-flag--dcore-lint], it checks the GH C and its runtime , not the user code
      Opt_DoCoreLinting
    , -- Convert typed hole errors into warnings, deferring the error until runtime.
      Opt_DeferTypedHoles
    , -- Turn type errors into warnings, deferring the error until runtime.
      Opt_DeferTypeErrors
    , -- defer the printing of error messages and warnings until the end of the compilation process and to group them by severity.
      Opt_DeferDiagnostics
    , -- GHC will build a mapping from info table pointers to source locations and some extra type information.
      -- track down memory leak using this : https://well-typed.com/blog/2021/01/first-look-at-hi-profiling-mode/
      Opt_InfoTableMap
    , -- automatically link in the base and rts packages.
      Opt_AutoLinkPackages
    ]

unsetGenFlags :: [GeneralFlag]

-- | List of general flags to disable
unsetGenFlags =
    [ Opt_KeepHiFiles
    , Opt_KeepOFiles
    ]

setWarnFlags :: [WarningFlag]

-- | List of warning flags to enable
setWarnFlags =
    [ -- pretty straight forward
      Opt_WarnOverlappingPatterns
    , Opt_WarnIncompletePatterns
    , -- must be used in conjunction with Opt_DeferTypedHoles
      Opt_WarnTypedHoles
    ]

unsetWarnFlags :: [WarningFlag]

-- | List of warning flags to disable
unsetWarnFlags =
    [ Opt_WarnUnrecognisedPragmas
    , Opt_WarnInlineRuleShadowing
    ]