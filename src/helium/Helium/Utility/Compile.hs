{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Helium.Utility.Compile (compile, AskelleOptions (..), IsPrelude, CompilationResult (..), askelleDefaultOptions, typeOf, HeliumError (..)) where

import Control.Monad (ap, liftM)
import Control.Monad.Except (ExceptT (..), MonadError (throwError), MonadTrans (lift), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadIO (..))
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing, listToMaybe, mapMaybe)
import qualified Data.Text as T
import System.FilePath (joinPath, takeDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)

import qualified Helium.Main.CompileUtils as Helium
import qualified Helium.Main.PhaseDesugarer as Helium
import qualified Helium.Main.PhaseImport as Helium
import qualified Helium.Main.PhaseLexer as Helium
import qualified Helium.Main.PhaseParser as Helium
import qualified Helium.Main.PhaseResolveOperators as Helium
import qualified Helium.Main.PhaseStaticChecks as Helium
import qualified Helium.Main.PhaseTypeInferencer as Helium
import qualified Helium.Main.PhaseTypingStrategies ()
import qualified Helium.ModuleSystem.DictionaryEnvironment as Helium
import qualified Helium.Parser.Lexer as HeliumParser
import qualified Helium.Parser.ParseLibrary as HeliumParser
import qualified Helium.Parser.Parser as HeliumParser
import qualified Helium.Parser.ResolveOperators as HeliumParser
import qualified Helium.StaticAnalysis.Messages.HeliumMessages as HeliumSA
import qualified Helium.StaticAnalysis.Messages.Messages as HeliumSA
import qualified Helium.StaticAnalysis.Messages.StaticErrors as HeliumSA
import qualified Helium.StaticAnalysis.Messages.TypeErrors as HeliumSA
import qualified Helium.StaticAnalysis.Messages.Warnings as HeliumSA
import qualified Helium.StaticAnalysis.Miscellaneous.TypeConversion as HeliumSA
import qualified Helium.Syntax.UHA_Range as Helium
import qualified Helium.Syntax.UHA_Syntax as Helium
import qualified Helium.Syntax.UHA_Utils as Helium
import qualified Helium.Utils.Utils as Helium
import qualified Top.Types as Top

import Helium.Utility.Instances ()

compile :: IsPrelude -> T.Text -> AskelleOptions -> IO (Either (HeliumError, T.Text) CompilationResult)
-- since we dont perfrom any side-effects, unsafePerformIO doesn't violate referential transparency rule.
compile isPrelude txt opts = do
    ea <- runExceptT $ runCompile $ compile' isPrelude txt [Helium.Overloading, Helium.UseTutor] opts
    case ea of
        Left (errTyp, errText) -> pure $ Left (errTyp, T.unlines errText)
        Right a -> pure $ Right a

newtype Compile a = MkCompile {runCompile :: ExceptT (HeliumError, [T.Text]) IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadError (HeliumError, [T.Text]))

data AskelleOptions = AskelleOptions
    { filterTypeSigs :: Bool
    -- ^ Filtering type signatures helps when student programs refer to type
    -- | synonyms that are not in scope
    , imports :: [(String, String)]
    -- ^ Each import is specified as a pair of a function name and a type signature.
    -- | For instance, you could import the isJust function using the tuple
    -- | ("isJust", "Maybe a -> Bool")
    }

-- By default, ignore the type signature and empty imports
askelleDefaultOptions :: AskelleOptions
askelleDefaultOptions = AskelleOptions True []

type IsPrelude = Bool

data CompilationResult = CompilationResult
    { getDictEnv :: Helium.DictionaryEnvironment
    , getImportEnv :: Helium.ImportEnvironment
    , getTypeEnv :: Helium.TypeEnvironment
    , getWarnings :: [HeliumSA.Warning]
    , getModule :: Helium.Module
    }

data HeliumError
    = HeliumLexerError
    | HeliumParserError
    | HeliumResolverError
    | HeliumStaticCheckError
    | HeliumTypeError
    deriving stock (Eq, Show)

data HeliumPhase
    = HeliumLexer
    | HeliumParser
    | HeliumResolver
    | HeliumStaticChecker
    | HeliumTypeChecker

-- really fragile :-(
getLvmPath :: IO [String]
getLvmPath = do
    (p : _) <- lines <$> readProcess "heliumpath" [] []
    pure [joinPath [takeDirectory p, "share", "lib"]]

compile' :: IsPrelude -> T.Text -> [Helium.Option] -> AskelleOptions -> Compile CompilationResult
compile' isPrelude codeSnippet heliumOptions askelleOptions = do
    lvmPath <- liftIO getLvmPath

    let AskelleOptions filterTypSigs imports = askelleOptions
        fullName = "Student"

    -- Phase 1 : Lexing
    (_lexWarnings, tokens) <-
        doPhaseWithExit HeliumLexer $
            Helium.phaseLexer fullName (T.unpack codeSnippet) heliumOptions

    -- Phase 2: Parsing
    parsedModule <-
        doPhaseWithExit HeliumParser $
            Helium.phaseParser fullName tokens heliumOptions

    -- Ignore type signatures if indicated in AskelleOptions
    -- Why? a hypothesis listed in weekly.md
    let parsedModule' = applyWhen filterTypSigs ignoreTypeSigs parsedModule

    -- Phase 3 : Importing
    -- We have a static import environment
    (_indirectionDecls, importEnvsWithMod) <-
        liftIO $ Helium.phaseImport fullName parsedModule' lvmPath []

    let ns = toplevelNames parsedModule
        importEnvs' = map (\(_, b, _) -> b) importEnvsWithMod
        importEnvs = toImportEnv imports : applyWhen isPrelude (filterImportEnvs ns) importEnvs'

    -- Phase 4 : Resolving Operators
    resolvedModule <-
        doPhaseWithExit HeliumResolver $
            Helium.phaseResolveOperators parsedModule' importEnvs heliumOptions

    -- Phase 5: Static checking
    (localEnv, _typeSignatures, staticWarnings) <-
        doPhaseWithExit HeliumStaticChecker $
            Helium.phaseStaticChecks fullName resolvedModule importEnvsWithMod heliumOptions

    -- Phase 6: Kind inferencing (skipped)
    let combinedEnv = foldr Helium.combineImportEnvironments localEnv importEnvs
    -- Phase 7: Type Inference Directives (skipped)
    let beforeTypeInferEnv = combinedEnv

    -- Phase 8 : Type Inferencing
    let newOptions =
            if null [() | HeliumSA.Shadow _ _ <- staticWarnings]
                then heliumOptions
                else Helium.NoOverloadingTypeCheck : heliumOptions

    (dictionaryEnv, afterTypeInferEnv, toplevelTypes, typeWarnings) <-
        doPhaseWithExit HeliumTypeChecker $
            Helium.phaseTypeInferencer "." fullName resolvedModule localEnv beforeTypeInferEnv newOptions

    pure $ CompilationResult dictionaryEnv afterTypeInferEnv toplevelTypes typeWarnings resolvedModule
  where
    -- base function, but not included until base-4.18.0.0
    applyWhen :: Bool -> (a -> a) -> a -> a
    applyWhen True f x = f x
    applyWhen False _ x = x

-- | Adjusted code from CompileUtils
doPhaseWithExit :: HeliumSA.HasMessage err => HeliumPhase -> Helium.Phase err a -> Compile a
doPhaseWithExit phase phaseFn = MkCompile . ExceptT $ do
    let constructErr HeliumLexer = HeliumLexerError
        constructErr HeliumParser = HeliumParserError
        constructErr HeliumResolver = HeliumResolverError
        constructErr HeliumStaticChecker = HeliumStaticCheckError
        constructErr HeliumTypeChecker = HeliumTypeError
    result <- phaseFn
    case result of
        Left errs -> do
            let errs' = [T.pack $ HeliumSA.sortAndShowMessages errs]
            pure (Left (constructErr phase, errs'))
        Right a -> pure $ pure a

-- | Remove type signatures from top-level functions and functions in where clauses
ignoreTypeSigs :: Helium.Module -> Helium.Module
ignoreTypeSigs (Helium.Module_Module r n e b) = Helium.Module_Module r n e (filterBody b)
  where
    filterBody h@(Helium.Body_Hole{}) = h
    filterBody (Helium.Body_Body r' i ds) = Helium.Body_Body r' i (filterDecls ds)
    filterDecls ds =
        -- First, walk through all function binding decls, because they may contain decls
        let ds' = map filterFunctionBindings ds
        in  -- Then remove type signatures
            filter (not . isTypeSig) ds'
    filterFunctionBindings (Helium.Declaration_FunctionBindings r' bindings) = Helium.Declaration_FunctionBindings r' (map filterFunctionBinding bindings)
    filterFunctionBindings d = d
    filterFunctionBinding (Helium.FunctionBinding_FunctionBinding r' lhs rhs) = Helium.FunctionBinding_FunctionBinding r' lhs (filterRhs rhs)
    filterFunctionBinding binding = binding
    filterRhs (Helium.RightHandSide_Expression r' e' (Helium.MaybeDeclarations_Just d)) = Helium.RightHandSide_Expression r' e' (Helium.MaybeDeclarations_Just $ filterDecls d)
    filterRhs rhs@(Helium.RightHandSide_Expression{}) = rhs
    filterRhs (Helium.RightHandSide_Guarded r' e' (Helium.MaybeDeclarations_Just d)) = Helium.RightHandSide_Guarded r' e' (Helium.MaybeDeclarations_Just $ filterDecls d)
    filterRhs rhs@(Helium.RightHandSide_Guarded{}) = rhs
    isTypeSig (Helium.Declaration_TypeSignature{}) = True
    isTypeSig _ = False

toplevelNames :: Helium.Module -> [Helium.Name]
toplevelNames (Helium.Module_Module _ _ _ b) =
    case b of
        Helium.Body_Hole{} -> []
        Helium.Body_Body _ _ ds -> mapMaybe declName ds
  where
    declName d = case d of
        Helium.Declaration_FunctionBindings _ fbs -> listToMaybe $ mapMaybe fbName fbs
        Helium.Declaration_PatternBinding _ p _ -> patName p
        _ -> Nothing

    fbName fb = case fb of
        Helium.FunctionBinding_Hole{} -> Nothing
        Helium.FunctionBinding_Feedback{} -> Nothing
        Helium.FunctionBinding_FunctionBinding _ lhs _ -> Just $ lhsName lhs

    patName p = case p of
        Helium.Pattern_Variable _ n -> Just n
        Helium.Pattern_Parenthesized _ p' -> patName p'
        _ -> Nothing

    lhsName lhs = case lhs of
        Helium.LeftHandSide_Function _ n _ -> n
        Helium.LeftHandSide_Infix _ _ op _ -> op
        Helium.LeftHandSide_Parenthesized _ lhs' _ -> lhsName lhs'

-- | Construct an import environment based on the list of imports
toImportEnv :: [(String, String)] -> Helium.ImportEnvironment
toImportEnv = foldr addType' Helium.emptyEnvironment
  where
    addType' (name, ty) = Helium.addType (Helium.Name_Identifier Helium.noRange [] [] name) (parseTpScheme ty)

-- | Parse a type from a string
parseTpScheme :: String -> Top.TpScheme
parseTpScheme = HeliumSA.makeTpSchemeFromType . parseFromString HeliumParser.contextAndType

-- | Adjusted code from Helium.ModuleSystem.CoreToImportEnv
parseFromString :: HeliumParser.HParser a -> String -> a
parseFromString p string =
    case HeliumParser.lexer [] "ParseFromString" string of
        Left _ -> Helium.internalError "ParseFromString" "parseFromString" ("lex error in " ++ string)
        Right (tokens, _) ->
            case HeliumParser.runHParser p "ParseFromString" tokens True {- wait for EOF -} of
                Left _ -> Helium.internalError "ParseFromString" "parseFromString" ("parse error in " ++ string)
                Right x -> x

parseFromString' :: [Helium.Option] -> HeliumParser.HParser a -> String -> Maybe a
parseFromString' options p string =
    case HeliumParser.lexer [] "ParseFromString" string of
        Left _ -> Nothing
        Right (tokens, _) ->
            case HeliumParser.runHParser p "ParseFromString" tokens True {- wait for EOF -} of
                Left _ -> Nothing
                Right x -> Just x

filterImportEnvs :: [Helium.Name] -> [Helium.ImportEnvironment] -> [Helium.ImportEnvironment]
filterImportEnvs ns = map f
  where
    f :: Helium.ImportEnvironment -> Helium.ImportEnvironment
    f env = env{Helium.typeEnvironment = Map.filterWithKey p (Helium.typeEnvironment env)}
    p :: Helium.Name -> tpScheme -> Bool
    p n _ = n `notElem` ns

typeOf :: (MonadError String m, MonadIO m) => String -> T.Text -> m T.Text
typeOf fun txt = do
    res <- liftIO $ compile False txt askelleDefaultOptions
    let typeEnv = either (const Map.empty) (Map.mapKeys show . getTypeEnv) res
    let fnTyp = Map.lookup fun typeEnv
    guardM (isNothing fnTyp) $ "We can't find type for" ++ show fun
    pure . T.pack . show $ fromJust fnTyp

--
guardM :: (MonadError e m) => Bool -> e -> m ()
guardM False err = throwError err
guardM True _ = pure ()
