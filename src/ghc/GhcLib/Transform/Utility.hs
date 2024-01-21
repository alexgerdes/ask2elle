
{-# LANGUAGE MonoLocalBinds #-}

module GhcLib.Transform.Utility where

-- GHC imports 
import qualified GHC as GHC 
import qualified GHC.Plugins as GHC 
import qualified GHC.Core.TyCo.Rep as GHC 
import qualified GHC.Utils.Outputable as GHC 
import qualified GHC.Core as GHC 
import qualified GHC.Core.Predicate as GHC
import qualified GHC.Types.Id.Info as GHC 
import qualified GHC.Hs as GHC 
import qualified GHC.Types.SrcLoc as GHC 
import qualified GHC.Types.Name.Occurrence as Occ
import qualified GHC.Utils.Encoding as GHC 

-- General imports 
import Data.Maybe ( isNothing, fromJust, catMaybes, isJust, mapMaybe )
import Data.Generics.Uniplate.Data
import Control.Monad (when)
import Data.Data ( Data )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text.Lazy (splitOn)
import Debug.Trace (trace)
import Data.Char (isSpace)
import Control.Monad.Identity (Identity(runIdentity))
import Debug.Trace 
import qualified GHC.Utils.Ppr as GHC
import System.IO (stdout)
import GHC.IO (unsafePerformIO)
-- Local imports 
-- import Instances.ShowAS


isHoleExpr :: GHC.CoreExpr -> Bool
-- | Check if a case expression is a typed hole expression
isHoleExpr (GHC.Case e _ t _) = hasHoleMsg e     -- need to check hasHoleMsg if deferring all type errors
isHoleExpr (GHC.Tick _ e)     = isHoleExpr e
isHoleExpr _              = False                -- and not only typed holes

isTyError :: GHC.CoreExpr -> Bool
isTyError (GHC.Case e _ t _) = case getTypErr e of
                          Just _ -> not (hasHoleMsg e) -- check if we have a type error that is not a hole
                          Nothing -> False
isTyError (GHC.Tick _ e)     = isTyError e
isTyError _              = False

hasHoleMsg :: GHC.CoreExpr -> Bool
hasHoleMsg e =  let ssdoc = GHC.interpp'SP  $ children e in 
  seq (unsafePerformIO $ GHC.printSDoc GHC.defaultSDocContext GHC.ZigZagMode stdout ssdoc) (not $ null [l | GHC.Lit l <- children e, isTypedHolErrMsg l])


isTypedHolErrMsg :: GHC.Literal -> Bool
isTypedHolErrMsg (GHC.LitString l) = f $ (lines $ GHC.utf8DecodeByteString l)
  where
    -- ! Next time, start here, not a big fan of comparing strings equality to determine if we have a typed hole
    -- !   At least, doing some parsing checks here
    f ls
        | length ls > 1 = "hole:" `elem` (words (ls !! 1))
        | otherwise = False
isTypedHolErrMsg _ = False


getTypErr :: GHC.CoreExpr -> Maybe GHC.Var
getTypErr = getVarFromName "typeError"

getPatErr :: GHC.CoreExpr -> Maybe GHC.Var
getPatErr = getVarFromName "patError"


getVarFromName :: String -> GHC.CoreExpr -> Maybe GHC.Var
getVarFromName name e
    | null vars = Nothing
    | otherwise = head vars -- just return first found variable if any matching
  where
    vars = [Just v | (GHC.Var v) <- universe e, GHC.getOccString v == name]


makeName :: String -> GHC.Unique -> GHC.SrcSpan -> GHC.Name

{- | Create a name from a string and a variable
   used for renaming variables
-}
makeName n uq loc = GHC.mkInternalName uq (GHC.mkOccName Occ.varName n) loc