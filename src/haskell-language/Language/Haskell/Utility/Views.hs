{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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
module Language.Haskell.Utility.Views
    ( -- * Views
      heliumView -- , ghcView

      -- * Conversion functions
    , toHelium
    , fromHelium
    , inHelium

      -- * Casting type class
    , Cast
    ) where

import Data.Char (readLitChar)
import Helium.Utility.Helium hiding (Body, Literal, MaybeName, Module, Name)
import qualified Helium.Utility.Helium as Helium
import Ideas.Common.Library hiding (from, to)
import Language.Haskell.Generated.Syntax

-- | Convert bewteen Helium and own abstract syntax
heliumView :: Cast a b => View a b
heliumView = makeView (Just . fromHelium) toHelium

fromHelium :: Cast a b => a -> b
fromHelium = from

toHelium :: Cast a b => b -> a
toHelium = to

inHelium :: Cast a b => (a -> a) -> b -> b
inHelium f = from . f . to

class Cast a b | a -> b, b -> a where
    from :: a -> b
    to :: b -> a

-- instance Cast Helium.Module Module where
--    from m =
--       case m of
--          Module_Module _ n _ (Body_Hole _ i)    -> Module (from n) []
--          Module_Module _ n _ (Body_Body _ _ ds) -> Module (from n) (map from ds)
--
--    to m =
--       case m of
--          Module n ds ->
--             Module_Module noRange (to n) MaybeExports_Nothing
--                (Body_Body noRange [] $ map to ds)
instance Cast Helium.Module Module where
    from m =
        case m of
            Module_Module _ _ _ b -> Module NoName (from b) -- ignore module for now

    to m =
        case m of
            Module n b ->
                Module_Module noRange (to n) MaybeExports_Nothing (to b)

instance Cast Helium.Body Body where
    from b =
        case b of
            Body_Hole _ _ -> BHole
            Body_Body _ _ ds -> Body $ map from ds

    to b =
        case b of
            BHole -> Body_Hole noRange "?"
            Body ds -> Body_Body noRange [] $ map to ds

instance Cast Declaration Decl where
    from d =
        case d of
            Declaration_Hole _ i -> DHole i
            Declaration_Empty _ -> DEmpty
            Declaration_FunctionBindings _ fs -> DFunBinds $ map from fs
            Declaration_PatternBinding _ p rhs -> DPatBind (from p) (from rhs)
            Declaration_TypeSignature{} -> DEmpty -- throw away type sigs
            Declaration_Fixity{} -> DEmpty -- throw away fixity decls
            Declaration_Data{} -> DEmpty
            _ -> castError d

    to d =
        case d of
            DHole i -> Declaration_Hole noRange i
            DEmpty -> Declaration_Empty noRange
            DFunBinds fs -> Declaration_FunctionBindings noRange $ map to fs
            DPatBind p rhs -> Declaration_PatternBinding noRange (to p) (to rhs)

instance Cast FunctionBinding FunBind where
    from f =
        case f of
            FunctionBinding_Hole _ i -> FBHole i
            FunctionBinding_FunctionBinding _ lhs rhs ->
                FunBind Nothing (from n') (map from ps') (from rhs)
              where
                (n', ps') = fromLhs lhs
                fromLhs (LeftHandSide_Function _ n ps) = (n, ps)
                fromLhs (LeftHandSide_Infix _ lp op rp) = (op, [lp, rp]) -- This is a shorcut (read: hack) we can't go back to infix!
                fromLhs _ = castError f
            FunctionBinding_Feedback _ s fb -> addFB s (from fb)
      where
        addFB s fb = case fb of
            FunBind _ a b c -> FunBind (Just s) a b c
            _ -> fb

    to f =
        case f of
            FBHole i -> FunctionBinding_Hole noRange i
            FunBind mfb n ps rhs ->
                maybe id (FunctionBinding_Feedback noRange) mfb $
                    FunctionBinding_FunctionBinding noRange lhs (to rhs)
              where
                lhs = LeftHandSide_Function noRange (to n) (map to ps)

instance Cast Expression Expr where
    from expr =
        case expr of
            Expression_Hole _ i -> Hole i
            Expression_Feedback _ s e -> Feedback s (from e)
            Expression_MustUse _ e -> MustUse (from e)
            Expression_Eta _ n e -> Eta n (from e)
            Expression_Case _ e as -> Case (from e) (map from as)
            Expression_Constructor _ n -> Con $ from n
            Expression_If _ c t e -> If (from c) (from t) (from e)
            Expression_InfixApplication _ mel e mer -> InfixApp (from mel) (from e) (from mer)
            Expression_Lambda _ ps e -> Lambda (map from ps) (from e)
            Expression_Let _ ds e -> Let (map from ds) (from e)
            Expression_Literal _ l -> Lit $ from l
            Expression_NormalApplication _ f es -> App (from f) (map from es)
            Expression_Parenthesized _ e -> Paren (from e)
            Expression_Tuple _ es -> Tuple $ map from es
            Expression_Variable _ n -> Var $ from n
            Expression_Enum _ e me1 me2 -> Enum (from e) (from me1) (from me2)
            Expression_List _ es -> List (map from es)
            Expression_Negate _ e -> Neg $ from e
            Expression_NegateFloat _ e -> Neg $ from e -- AG: hack we can't go back!!!
            Expression_Comprehension _ e qs -> desugarComprehension e qs
            _ -> castError expr

    to expr =
        case expr of
            Hole i -> Expression_Hole noRange i
            Feedback s e -> Expression_Feedback noRange s (to e)
            MustUse e -> to e
            Refactor e _ -> to e -- Remove any alternatives
            Eta _ e -> to e
            Case e as -> Expression_Case noRange (to e) (map to as)
            Con n -> Expression_Constructor noRange $ to n
            If c t e -> Expression_If noRange (to c) (to t) (to e)
            InfixApp mel e mer -> Expression_InfixApplication noRange (to mel) (to e) (to mer)
            Lambda ps e -> Expression_Lambda noRange (map to ps) (to e)
            Let ds e -> Expression_Let noRange (map to ds) (to e)
            Lit l -> Expression_Literal noRange $ to l
            App f es -> Expression_NormalApplication noRange (to f) (map to es)
            Paren e -> Expression_Parenthesized noRange $ to e
            Tuple es -> Expression_Tuple noRange $ map to es
            Var n -> Expression_Variable noRange $ to n
            Enum e me1 me2 -> Expression_Enum noRange (to e) (to me1) (to me2)
            List es -> Expression_List noRange $ map to es
            Neg e -> Expression_Negate noRange $ to e

-- See https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-420003.11
-- Note that this does not offer proper list comprehension support (i.e. Ask-Elle
-- will hang whenever asked for feedback). However, it is enough when you are only
-- interested in normalizing
desugarComprehension :: Expression -> [Qualifier] -> Expr
desugarComprehension e [] = List [from e]
desugarComprehension e (q : qs) = case q of
    -- [ e | b, Q ] = if b then [  e | Q ] else []
    Qualifier_Guard _ b -> If (from b) e_Q (List [])
    -- [ e | let decls, Q ] = let decls in [ e | Q ]
    Qualifier_Let _ decls -> Let (map from decls) e_Q
    -- [ e | p <- l, Q ] = let ok p = [ e | Q ]
    --                         ok _ = []
    --                     in concatMap ok  l
    Qualifier_Generator _ pattern expr ->
        -- Note: the fname hack will work as long as it doesn't shadow a free variable in e or l
        let fname = Ident "this-is-a-hack-and-should-be-something-unique"
            pMatch = FunBind Nothing fname [from pattern] (Rhs e_Q [])
            pNoMatch = FunBind Nothing fname [PWildcard] (Rhs (List []) [])
            letExpr = App (Var fname) [from expr]
        in  Let [DFunBinds [pMatch, pNoMatch]] letExpr
    Qualifier_Empty _ -> error "Empty qualifier in list comprehension"
  where
    e_Q = desugarComprehension e qs

instance Cast GuardedExpression GuardedExpr where
    from (GuardedExpression_GuardedExpression _ g e) = GExpr (from g) (from e)

    to gexpr =
        case gexpr of
            GExpr g e ->
                GuardedExpression_GuardedExpression noRange (to g) (to e)

instance Cast Pattern Pat where
    from pat =
        case pat of
            Pattern_Hole _ i
                | i == rEFACTOR_ARGS_NAME -> PMultipleHole i
                | otherwise -> PHole i
            Pattern_Constructor _ n ps -> PCon (from n) (map from ps)
            Pattern_InfixConstructor _ pl n pr -> PInfixCon (from pl) (from n) (from pr)
            Pattern_List _ ps -> PList (map from ps)
            Pattern_Literal _ l -> PLit (from l)
            Pattern_Parenthesized _ p -> PParen (from p)
            Pattern_Tuple _ ps -> PTuple (map from ps)
            Pattern_Variable _ n -> PVar (from n)
            Pattern_As _ n p -> PAs (from n) (from p)
            Pattern_Wildcard _ -> PWildcard
            _ -> castError pat

    to pat =
        case pat of
            PHole i -> Pattern_Hole noRange i
            PMultipleHole i -> Pattern_Hole noRange i
            PCon n ps -> Pattern_Constructor noRange (to n) (map to ps)
            PInfixCon pl n pr -> Pattern_InfixConstructor noRange (to pl) (to n) (to pr)
            PList ps -> Pattern_List noRange $ map to ps
            PLit l -> Pattern_Literal noRange $ to l
            PParen p -> Pattern_Parenthesized noRange $ to p
            PTuple ps -> Pattern_Tuple noRange $ map to ps
            PVar n -> Pattern_Variable noRange $ to n
            PAs n p -> Pattern_As noRange (to n) (to p)
            PWildcard -> Pattern_Wildcard noRange

instance Cast Helium.Literal Literal where
    from l =
        case l of
            Literal_Char _ s -> LChar $ fst $ head $ readLitChar s
            Literal_Float _ s -> LFloat $ read s
            Literal_Int _ s -> LInt $ read s
            Literal_String _ s -> LString s

    to l =
        case l of
            LChar c -> Literal_Char noRange [c]
            LFloat f -> Literal_Float noRange $ show f
            LInt i -> Literal_Int noRange $ show i
            LString s -> Literal_String noRange s

instance Cast RightHandSide Rhs where
    from rhs =
        case rhs of
            RightHandSide_Expression _ e mds -> Rhs (from e) (from mds)
            RightHandSide_Guarded _ ges mds -> GRhs (map from ges) (from mds)

    to rhs =
        case rhs of
            Rhs e ds -> RightHandSide_Expression noRange (to e) (to ds)
            GRhs ges ds -> RightHandSide_Guarded noRange (map to ges) (to ds)

instance Cast MaybeDeclarations [Decl] where
    from mds =
        case mds of
            MaybeDeclarations_Just ds -> map from ds
            MaybeDeclarations_Nothing -> []

    to [] = MaybeDeclarations_Nothing
    to ds = MaybeDeclarations_Just $ map to ds

instance Cast Helium.MaybeName MaybeName where
    from mn =
        case mn of
            MaybeName_Just n -> JustName $ from n
            MaybeName_Nothing -> NoName

    to mn =
        case mn of
            JustName n -> MaybeName_Just $ to n
            NoName -> MaybeName_Nothing

instance Cast MaybeExpression MaybeExpr where
    from me =
        case me of
            MaybeExpression_Just e -> JustExpr $ from e
            MaybeExpression_Nothing -> NoExpr

    to me =
        case me of
            JustExpr e -> MaybeExpression_Just $ to e
            NoExpr -> MaybeExpression_Nothing

instance Cast Helium.Name Name where
    from n =
        case n of
            Name_Identifier _ _ _ s -> Ident s
            Name_Operator _ _ _ s -> Operator s
            Name_Special _ _ _ s -> Special s

    to n =
        case n of
            Ident s -> Name_Identifier noRange [] [] s
            Operator s -> Name_Operator noRange [] [] s
            Special s -> Name_Special noRange [] [] s

instance Cast Alternative Alt where
    from a =
        case a of
            Alternative_Hole _ i -> AHole i
            Alternative_Alternative _ p rhs -> Alt Nothing (from p) (from rhs)
            Alternative_Feedback _ s al -> addFB s (from al)
            Alternative_Empty _ -> AltEmpty
      where
        addFB s al = case al of
            Alt _ x y -> Alt (Just s) x y
            _ -> al

    to a =
        case a of
            AHole i -> Alternative_Hole noRange i
            Alt mfb p rhs ->
                maybe id (Alternative_Feedback noRange) mfb $
                    Alternative_Alternative noRange (to p) (to rhs)
            AltEmpty -> Alternative_Empty noRange

-- | Help functions
castError :: Show a => a -> b
castError x = error $ "Unable to cast: " ++ show x
