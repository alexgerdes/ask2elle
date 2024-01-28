
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE BlockArguments #-}

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
import Control.Monad (when, void)
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
import qualified Text.Megaparsec as Parser
import qualified Text.Megaparsec.Char as Parser
import Data.Void (Void)
import Control.Monad (replicateM_)


-- * Functions for checking if an expression is a typed hole
isHoleExpr :: GHC.CoreExpr -> Bool
-- | Check if a case expression is a typed hole expression
isHoleExpr (GHC.Case e _ _ _) = hasHoleMsg e     -- need to check hasHoleMsg if deferring all type errors
isHoleExpr (GHC.Tick _ e)     = isHoleExpr e
isHoleExpr _              = False                -- and not only typed holes

-- | Take a GHC CoreExpr and check if it contains a hole message
hasHoleMsg :: GHC.CoreExpr -> Bool
hasHoleMsg e = not $ null [l | GHC.Lit l <- children e, isTypedHolErrMsg l]

-- | Take a GHC Literal and check if it is a typed hole error message
isTypedHolErrMsg :: GHC.Literal -> Bool
isTypedHolErrMsg (GHC.LitString l) = 
  let litStr = lines $ GHC.utf8DecodeByteString l
      checkHoleMsg :: [String] -> Bool 
      checkHoleMsg xs
        -- The LitString could be just a regular  oneline string 
        | length xs >= 2 = let holeMsg = xs !! 1 
                           -- The LitString could be a multiline string, but without the hole message
                           in  case Parser.parseMaybe parseHoleErrStr holeMsg of 
                                        Nothing -> False
                                        Just () -> True
        | otherwise = False
  in checkHoleMsg litStr
  where       
      parseHoleErrStr :: Parser.Parsec Void String ()
      parseHoleErrStr = do
        replicateM_ 4 Parser.space
        void $ Parser.string "• Found hole: _ ::"
        void $ Parser.many $ Parser.satisfy (const True) 
        Parser.eof
isTypedHolErrMsg _ = False

-- * Functions for checking if an expression is a pattern error
isPatError :: GHC.CoreExpr -> Bool
isPatError (GHC.Case e _ t _) = case getPatErr e of
    Just pe -> True
    _ -> False
isPatError (GHC.Tick _ e) = isPatError e
isPatError e = isPatErrVar e

isPatErrVar :: GHC.CoreExpr -> Bool
isPatErrVar (GHC.Var v) = isErrVar "patError" v
isPatErrVar _ = False

-- * Utility functions for checking if an expression fits a certain pattern
getTypErr :: GHC.CoreExpr -> Maybe GHC.Var
getTypErr = getVarFromName "typeError"

getPatErr :: GHC.CoreExpr -> Maybe GHC.Var
getPatErr = getVarFromName "patError"

isErrVar :: String -> GHC.Var -> Bool
isErrVar s v = GHC.getOccString v == s

getVarFromName :: String -> GHC.CoreExpr -> Maybe GHC.Var
getVarFromName name e
    | null vars = Nothing
    -- ? this seems a hacky way to get the right variable, no idea it works for every situation
    | otherwise = head vars -- just return first found variable if any matching
  where
    vars = [Just v | (GHC.Var v) <- children e, GHC.getOccString v == name]


makeName :: String -> GHC.Unique -> GHC.SrcSpan -> GHC.Name
{- | Create a name from a string and a variable
   used for renaming variables
-}
makeName n uq loc = GHC.mkInternalName uq (GHC.mkOccName Occ.varName n) loc


-- >>> test 
-- "Failed to parse"
