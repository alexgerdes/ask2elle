module Helium.Utility.PrettyPrinter where

import Data.Map qualified as Map
import Helium.Main.CompileUtils qualified as Helium
import Helium.Syntax.UHA_Pretty qualified as PP
import Helium.Syntax.UHA_Syntax qualified as Helium

import Data.Text qualified as T

-- | a Module pretty printer
ppModule :: Helium.Module -> T.Text
ppModule m =
    T.pack . show $ PP.text_Syn_Module (PP.wrap_Module (PP.sem_Module m) PP.Inh_Module)

ppDeclaration :: Helium.Declaration -> T.Text
ppDeclaration d =
    T.pack . show $
        PP.text_Syn_Declaration (PP.wrap_Declaration (PP.sem_Declaration d) PP.Inh_Declaration)

ppExpression :: Helium.Expression -> T.Text
ppExpression e =
    T.pack . show $
        PP.text_Syn_Expression (PP.wrap_Expression (PP.sem_Expression e) PP.Inh_Expression)
