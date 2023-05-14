module Helium.Utility.PrettyPrinter where

import Data.Map qualified as Map
import Helium.Main.CompileUtils qualified as Helium
import Helium.Syntax.UHA_Pretty qualified as PP
import Helium.Syntax.UHA_Syntax qualified as Helium

-- | a Module pretty printer
ppModule :: Helium.Module -> String
ppModule m =
    show $
        PP.text_Syn_Module (PP.wrap_Module (PP.sem_Module m) PP.Inh_Module)

ppDeclaration :: Helium.Declaration -> String
ppDeclaration d =
    show $
        PP.text_Syn_Declaration (PP.wrap_Declaration (PP.sem_Declaration d) PP.Inh_Declaration)

ppExpression :: Helium.Expression -> String
ppExpression e =
    show $
        PP.text_Syn_Expression (PP.wrap_Expression (PP.sem_Expression e) PP.Inh_Expression)

filterImportEnvs :: [Helium.Name] -> [Helium.ImportEnvironment] -> [Helium.ImportEnvironment]
filterImportEnvs ns = map f
  where
    f :: Helium.ImportEnvironment -> Helium.ImportEnvironment
    f env = env{Helium.typeEnvironment = Map.filterWithKey p (Helium.typeEnvironment env)}
    p :: Helium.Name -> tpScheme -> Bool
    p n _ = n `notElem` ns