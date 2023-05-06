-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Maintainer  :  alex@botkes.nl
-- Stability   :  provisional
-- Portability :  unknown
--
-- This module serves as a wrapper for the Helium compiler.
module Helium.Helium
    ( -- * Compile functions
      compile
    , unsafeCompile
    , compile'
    , compile_
    , compilePrelude

      -- * Compile options
    , AskelleOptions (..)
    , defaultOptions
    , Option (..)

      -- * Helium syntax data types
    , module Helium.Syntax.UHA_Syntax
    , module Helium.Syntax.UHA_Range

      -- * Pretty printing
    , ppModule
    , ppDeclaration
    , ppExpression

      -- * Miscellaneous functions
    , patternVars
    , phaseDesugarer
    --   -- * Class instances
    -- , module Language.Haskell.Compiler.Helium.Instances
    , parseFromString
    , parseFromString'
    )
where

import Helium.Main.CompileUtils hiding (doPhaseWithExit)
import Helium.Main.PhaseDesugarer
import Helium.Main.PhaseImport
import Helium.Main.PhaseLexer
import Helium.Main.PhaseParser
import Helium.Main.PhaseResolveOperators
import Helium.Main.PhaseStaticChecks
import Helium.Main.PhaseTypeInferencer
import Helium.Main.PhaseTypingStrategies ()
import Helium.ModuleSystem.DictionaryEnvironment
import Helium.Parser.Lexer (lexer)
import Helium.Parser.ParseLibrary (HParser, runHParser)
import Helium.Parser.Parser qualified as Parser
import Helium.StaticAnalysis.Messages.HeliumMessages
import Helium.StaticAnalysis.Messages.Messages
import Helium.StaticAnalysis.Messages.Warnings (Warning (Shadow))
import Helium.StaticAnalysis.Miscellaneous.TypeConversion (makeTpSchemeFromType)
import Helium.Syntax.UHA_Pretty qualified as PP
import Helium.Syntax.UHA_Range (noRange)
import Helium.Syntax.UHA_Syntax
import Helium.Syntax.UHA_Utils
import Helium.Utils.Utils (internalError)
import Top.Types (TpScheme)

import Control.Monad (ap, liftM)
import Control.Monad.Trans
import Data.Map qualified as M
import Data.Maybe
import System.FilePath (joinPath, takeDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)

import Helium.Utility.Instances ()

-- | a Module pretty printer
ppModule :: Module -> String
ppModule m =
    show $
        PP.text_Syn_Module (PP.wrap_Module (PP.sem_Module m) PP.Inh_Module)

ppDeclaration :: Declaration -> String
ppDeclaration d =
    show $
        PP.text_Syn_Declaration (PP.wrap_Declaration (PP.sem_Declaration d) PP.Inh_Declaration)

ppExpression :: Expression -> String
ppExpression e =
    show $
        PP.text_Syn_Expression (PP.wrap_Expression (PP.sem_Expression e) PP.Inh_Expression)

filterImportEnvs :: [Name] -> [ImportEnvironment] -> [ImportEnvironment]
filterImportEnvs ns = map f
  where
    f :: ImportEnvironment -> ImportEnvironment
    f env = env{typeEnvironment = M.filterWithKey p (typeEnvironment env)}
    p :: Name -> tpScheme -> Bool
    p n _ = n `notElem` ns

toplevelNames :: Module -> [Name]
toplevelNames (Module_Module _ _ _ b) =
    case b of
        Body_Hole _ _ -> []
        Body_Body _ _ ds -> mapMaybe declName ds
  where
    declName d = case d of
        Declaration_FunctionBindings _ fbs -> listToMaybe $ mapMaybe fbName fbs
        Declaration_PatternBinding _ p _ -> patName p
        _ -> Nothing

    fbName fb = case fb of
        FunctionBinding_Hole _ _ -> Nothing
        FunctionBinding_Feedback _ _ _ -> Nothing
        FunctionBinding_FunctionBinding _ lhs _ -> Just $ lhsName lhs

    patName p = case p of
        Pattern_Variable _ n -> Just n
        Pattern_Parenthesized _ p' -> patName p'
        _ -> Nothing

    lhsName lhs = case lhs of
        LeftHandSide_Function _ n _ -> n
        LeftHandSide_Infix _ _ op _ -> op
        LeftHandSide_Parenthesized _ lhs' _ -> lhsName lhs'

-- | the compiler/parser
unsafeCompile :: String -> Module
unsafeCompile s = either (error "Helium compilation error!") id $ compile s defaultOptions

compile :: String -> AskelleOptions -> Either String Module
compile source = fmap (\(_, _, _, _, m) -> m) . compile' False source

compilePrelude :: String -> Either String Module
compilePrelude s = fmap (\(_, _, _, _, m) -> m) $ compile' True s defaultOptions

-- | Helium compiler
-- compile' :: String -> Either String Module
compile'
    :: Bool
    -> String
    -> AskelleOptions
    -> Either
        String
        ( DictionaryEnvironment
        , ImportEnvironment
        , TypeEnvironment
        , [Warning]
        , Module
        )
compile' isPrelude txt opts = unsafePerformIO $ do
    lvmPath <- getLvmPath
    ea <- run $ compile_ isPrelude txt [Overloading, UseTutor] opts lvmPath
    case ea of
        Left ms -> return $ Left $ unlines ms
        Right a -> return $ Right a

-- really fragile :-(
getLvmPath :: IO [String]
getLvmPath = do
    (p : _) <- fmap lines $ readProcess "heliumpath" [] []
    return [joinPath [takeDirectory p, "share", "lib"]]

newtype Compile a = C {run :: IO (Either [String] a)}

instance Functor Compile where
    fmap = liftM

instance Applicative Compile where
    pure = C . return . Right
    (<*>) = ap

instance Monad Compile where
    return = pure
    C m >>= f = C $ do
        ea <- m
        case ea of
            Left err -> return (Left err)
            Right a -> do
                let C m2 = f a
                m2

instance MonadIO Compile where
    liftIO m = C $ do
        a <- m
        return (Right a)

data AskelleOptions = AskelleOptions
    { filterTypeSigs :: Bool
    -- ^ Filtering type signatures helps when student programs refer to type
    -- | synonyms that are not in scope
    , imports :: [(String, String)]
    -- ^ Each import is specified as a pair of a function name and a type signature.
    -- | For instance, you could import the isJust function using the tuple
    -- | ("isJust", "Maybe a -> Bool")
    }

defaultOptions :: AskelleOptions
defaultOptions = AskelleOptions True []

-- | Adjusted code from Compile
compile_
    :: Bool
    -> String
    -> [Option]
    -> AskelleOptions
    -> [String]
    -> Compile
        ( DictionaryEnvironment
        , ImportEnvironment
        , TypeEnvironment
        , [Warning]
        , Module
        )
compile_ isPrelude contents cliOptions compileOptions lvmPath =
    do
        let AskelleOptions filterTpSigs imps = compileOptions
        let fullName = "Student"

        -- Phase 1: Lexing
        -- (lexerWarnings, tokens) <-
        (_, tokens) <-
            doPhaseWithExit $
                phaseLexer fullName contents cliOptions

        -- Phase 2: Parsing
        parsedModule' <-
            doPhaseWithExit $
                phaseParser fullName tokens cliOptions

        -- Ignore type signatures if specified
        let parsedModule = when' filterTpSigs ignoreTypeSigs parsedModule'

        -- Phase 3: Importing
        -- We have a static import environment
        (_indirectionDecls, importEnvsWithMod) <-
            liftIO $ phaseImport fullName parsedModule lvmPath []

        let ns = toplevelNames parsedModule
            importEnvs' = map (\(_, b, _) -> b) importEnvsWithMod
            importEnvs = toImportEnv imps : when' isPrelude (filterImportEnvs ns) importEnvs'

        -- Phase 4: Resolving operators
        resolvedModule <-
            doPhaseWithExit $
                phaseResolveOperators parsedModule importEnvs cliOptions

        -- Phase 5: Static checking
        -- (localEnv, typeSignatures, staticWarnings) <-
        (localEnv, _, staticWarnings) <-
            doPhaseWithExit $
                phaseStaticChecks fullName resolvedModule importEnvsWithMod cliOptions

        -- Phase 6: Kind inferencing (skipped)
        let combinedEnv = foldr combineImportEnvironments localEnv importEnvs
        -- Phase 7: Type Inference Directives (skipped)
        let beforeTypeInferEnv = combinedEnv

        -- Phase 8: Type inferencing
        let newOptions =
                if null [() | Shadow _ _ <- staticWarnings]
                    then cliOptions
                    else NoOverloadingTypeCheck : cliOptions
        (dictionaryEnv, afterTypeInferEnv, toplevelTypes, typeWarnings) <-
            doPhaseWithExit $
                phaseTypeInferencer "." fullName resolvedModule localEnv beforeTypeInferEnv newOptions

        return (dictionaryEnv, afterTypeInferEnv, toplevelTypes, typeWarnings, resolvedModule)
  where
    when' :: Bool -> (a -> a) -> a -> a
    when' p f a = if p then f a else a

-- | Adjusted code from CompileUtils
doPhaseWithExit :: HasMessage err => Phase err a -> Compile a
doPhaseWithExit phase = C $
    do
        result <- phase
        case result of
            Left errs ->
                return (Left [sortAndShowMessages errs])
            Right a ->
                return (Right a)

-- | Construct an import environment based on the list of imports
toImportEnv :: [(String, String)] -> ImportEnvironment
toImportEnv = foldr addType' emptyEnvironment
  where
    addType' (name, ty) env = addType (Name_Identifier noRange [] [] name) (parseTpScheme ty) env

-- | Parse a type from a string
parseTpScheme :: String -> TpScheme
parseTpScheme = makeTpSchemeFromType . parseFromString Parser.contextAndType

-- | Adjusted code from Helium.ModuleSystem.CoreToImportEnv
parseFromString :: HParser a -> String -> a
parseFromString p string =
    case lexer [] "ParseFromString" string of
        Left _ -> internalError "ParseFromString" "parseFromString" ("lex error in " ++ string)
        Right (tokens, _) ->
            case runHParser p "ParseFromString" tokens True {- wait for EOF -} of
                Left _ -> internalError "ParseFromString" "parseFromString" ("parse error in " ++ string)
                Right x -> x

parseFromString' :: [Option] -> HParser a -> String -> Maybe a
parseFromString' options p string =
    case lexer options "ParseFromString" string of
        Left _ -> Nothing
        Right (tokens, _) ->
            case runHParser p "ParseFromString" tokens True {- wait for EOF -} of
                Left _ -> Nothing
                Right x -> Just x

-- | Remove type signatures from top-level functions and functions in where clauses
ignoreTypeSigs :: Module -> Module
ignoreTypeSigs (Module_Module r n e b) = Module_Module r n e (filterBody b)
  where
    filterBody h@(Body_Hole _ _) = h
    filterBody (Body_Body r' i ds) = Body_Body r' i (filterDecls ds)
    filterDecls ds =
        -- First, walk through all function binding decls, because they may contain decls
        let ds' = map filterFunctionBindings ds
        in  -- Then remove type signatures
            filter (not . isTypeSig) ds'
    filterFunctionBindings (Declaration_FunctionBindings r' bindings) = Declaration_FunctionBindings r' (map filterFunctionBinding bindings)
    filterFunctionBindings d = d
    filterFunctionBinding (FunctionBinding_FunctionBinding r' lhs rhs) = FunctionBinding_FunctionBinding r' lhs (filterRhs rhs)
    filterFunctionBinding binding = binding
    filterRhs (RightHandSide_Expression r' e' (MaybeDeclarations_Just d)) = RightHandSide_Expression r' e' (MaybeDeclarations_Just $ filterDecls d)
    filterRhs rhs@(RightHandSide_Expression _ _ _) = rhs
    filterRhs (RightHandSide_Guarded r' e' (MaybeDeclarations_Just d)) = RightHandSide_Guarded r' e' (MaybeDeclarations_Just $ filterDecls d)
    filterRhs rhs@(RightHandSide_Guarded _ _ _) = rhs
    isTypeSig (Declaration_TypeSignature{}) = True
    isTypeSig _ = False
