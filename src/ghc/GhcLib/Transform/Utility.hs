
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module GhcLib.Transform.Utility where

-- GHC imports 
import qualified GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Core.TyCo.Rep as GHC
import qualified GHC.Types.Name.Occurrence as Occ
import qualified GHC.Utils.Encoding as GHC

-- General imports 
import Data.Generics.Uniplate.Data
import Control.Monad ( void, replicateM_ )

import qualified Text.Megaparsec as Parser
import qualified Text.Megaparsec.Char as Parser
import Data.Void (Void)
import GHC.Base (assert)
import Data.Foldable (Foldable(foldl'))
import Data.Bifunctor (Bifunctor(first, second))


data HoleCandidates = HoleCandidates {
    holeCount :: !Int,
    holeNameCandicate :: ![GHC.UniqSupply]
  }

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
        -- Hole Msgs are always more than 2 lines, we need to check if the second line indicates it is a hole message
        | length xs >= 2 = let holeMsg = xs !! 1
                           in  case Parser.parseMaybe parseHoleErrStr holeMsg of
                                        -- The LitString could be a multiline string, but without the hole message
                                        Nothing -> False
                                        Just () -> True
        -- The LitString could be just a regular oneline string 
        | otherwise = False
  in checkHoleMsg litStr
  where
      parseHoleErrStr :: Parser.Parsec Void String ()
      parseHoleErrStr = do
        replicateM_ 4 Parser.space  -- ! This is hardcoded and might not work if the GHC error message output changes
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

{- 
-- * Alternative implementation of the functions for checking if an expression is a pattern error
isPatError :: GHC.CoreExpr -> Bool
-- | Check if a case expression is a typed hole expression
isPatError (GHC.Case e _ _ _) = hasPatErrMsg e     -- need to check hasHoleMsg if deferring all type errors
isPatError (GHC.Tick _ e)     = isPatError e
isPatError _              = False                -- and not only typed holes

-- | Take a GHC CoreExpr and check if it contains a hole message
hasPatErrMsg :: GHC.CoreExpr -> Bool
hasPatErrMsg e = not $ null [l | GHC.Lit l <- children e, isPatErrMsg l]

-- | Take a GHC Literal and check if it is a typed hole error message
isPatErrMsg :: GHC.Literal -> Bool
isPatErrMsg (GHC.LitString l) = 
  let litStr = GHC.utf8DecodeByteString l
      checkErrMsg :: String -> Bool 
      checkErrMsg xs = 
                      case Parser.parseMaybe parsePatErrStr xs of 
                                        Nothing -> False
                                        Just () -> True
  in checkErrMsg litStr
  where       
      parsePatErrStr :: Parser.Parsec Void String ()
      parsePatErrStr = do
        _ <- manyTill Parser.anySingle $ Parser.string "|case"
        Parser.eof
      manyTill :: Alternative m => m a -> m end -> m [a]
      manyTill p end = go
        where
          go = ([] <$ end) <|> ((:) <$> p <*> go)
isPatErrMsg _ = False
-}

getPatErr :: GHC.CoreExpr -> Maybe GHC.Var
getPatErr = getVarFromName "patError"


getTypErr :: GHC.CoreExpr -> Maybe GHC.Var
getTypErr = getVarFromName "typeError"

getVarFromName :: String -> GHC.CoreExpr -> Maybe GHC.Var
getVarFromName name e
    | null vars = Nothing
    -- ? this seems a hacky way to get the right variable, no idea it works for every situation
    | otherwise = head vars -- just return first found variable if any matching
  where
    vars = [Just v | (GHC.Var v) <- universe e, GHC.getOccString v == name]


isPatErrVar :: GHC.CoreExpr -> Bool
isPatErrVar (GHC.Var v) = isErrVar "patError" v
isPatErrVar _ = False

-- * Utility functions for checking if an expression fits a certain pattern
isErrVar :: String -> GHC.Var -> Bool
isErrVar s v = GHC.getOccString v == s

isSpecVar :: GHC.Var -> Bool
isSpecVar v = "$" == take 1 (GHC.getOccString v)


isTyConApp :: GHC.Type -> Bool
isTyConApp (GHC.TyConApp _ _) = True
isTyConApp _ = False


makeLocal :: GHC.Var -> GHC.Var
  -- | Invariant: in the call site, the input variable should always to a computational related value
  -- |    hence never type level value 
makeLocal v = assert (GHC.isId v) $ GHC.mkLocalId (GHC.varName v) (GHC.varMult v) (GHC.varType v)
makeName :: String -> GHC.Unique -> GHC.SrcSpan -> GHC.Name
{- | Create a name from a string and a variable
   used for renaming variables
-}
makeName n uq = GHC.mkInternalName uq (GHC.mkOccName Occ.varName n)

updateVar :: GHC.Var -> GHC.CoreBind -> GHC.CoreBind
-- | update variable information
updateVar v = transformBi $ \e -> case e :: GHC.CoreExpr of
    (GHC.Var v') | v == v' -> GHC.Var v
    rest -> rest


{- | substitution takes a replacement variable and a target variable and a CoreExpr,
       replacing every occurance of the target variable with the replacement variable in the CoreExpr
-}
substitution :: GHC.Var -> GHC.Var -> GHC.CoreExpr -> GHC.CoreExpr
substitution replacement target =
    -- trace ("found substitution" ++ show "["++ show v' ++ "->" ++ show v ++"]" ) $
    transformBi (sub replacement target)
    where
      sub :: GHC.Var -> GHC.Var -> GHC.CoreExpr -> GHC.CoreExpr
      -- | Replace the second variable with the first one given
      sub r t expr = case expr of
                        (GHC.Var ident) | ident == t -> GHC.Var r
                        e -> e

getBindTopVar :: GHC.CoreBind -> GHC.Var
{- |  Get variable of a binder
   |  Invariant : The input CoreBind will always return a var, even though the type of Rec indicates the possibility of empty list
-}
getBindTopVar (GHC.NonRec v _) = v
getBindTopVar (GHC.Rec xs ) = assert (not (null xs)) $ fst $ head xs


varAppearsInExpr :: GHC.Var -> GHC.CoreBind -> Bool
-- | Find out whether or not a variable is used somewhere in a binder
varAppearsInExpr  var expr = or [v == var | v <- universeBi expr]

getBindersByVar :: [GHC.CoreBind] -> GHC.CoreBind -> ([GHC.CoreBind], [GHC.CoreBind])
  -- | Get all binders referencing the second argument
getBindersByVar binds target = foldl' (\acc bind -> if isUsed bind then first (bind :) acc else second (bind :) acc) ([],[]) binds
  where
      isUsed :: GHC.CoreBind -> Bool
      isUsed = varAppearsInExpr (getBindTopVar target)
