{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Language.Haskell.Utility.SyntaxInstances where

import Control.Monad
import Ideas.Common.Environment
import Ideas.Common.Rewriting.Term
import Ideas.Common.Rewriting.Term.Decoder
import Ideas.Common.View
import Data.Char
import Data.Data
import Data.Generics.Uniplate.Direct
import Test.QuickCheck
import Language.Haskell.Generated.Syntax


-- | Symbols do not allow for capitals. We replace capitals by the corresponding
--   lower case letter with a preceding '-' character. A dash character is not 
--   allowed in constructor names.
constrToSym :: String -> Symbol
constrToSym = 
    newSymbol . foldr (\c cs -> if isUpper c then '-' : c : cs else c :cs) []

symToConstr :: Symbol -> String
symToConstr = 
    foldr (\c cs -> if c == '-' then capitalise cs else c:cs) [] . show
  where
    capitalise []     = []
    capitalise (x:xs) = toUpper x : xs

constrView :: View String Symbol
constrView = makeView (Just . constrToSym) symToConstr

genConstr :: Gen String
genConstr = liftM2 (:) capital (listOf (oneof [letter, capital, return '_']))
  where
    capital = choose ('A', 'Z')
    letter  = choose ('a', 'z')

testConstrView :: IO ()
testConstrView = quickCheck $ propSoundness (==) genConstr constrView

-- | Term instances for syntax datatypes
tCon0' :: Symbol -> a -> TermDecoder a
tCon0' s c = c <$ tCon0 s
tCon1' :: IsTerm a => Symbol -> (a -> b) -> TermDecoder b
tCon1' s c = tCon1 s c termDecoder
tCon2' :: (IsTerm a, IsTerm b) => Symbol -> (a -> b -> c) -> TermDecoder c
tCon2' s c = tCon2 s c termDecoder termDecoder
tCon3' :: (IsTerm a, IsTerm b, IsTerm c) => Symbol -> (a -> b -> c -> d) -> TermDecoder d
tCon3' s c = tCon3 s c termDecoder termDecoder termDecoder

sModule :: Symbol
sModule = newSymbol "Module"

instance IsTerm Module where
  toTerm (Module name body) = TCon sModule [toTerm name, toTerm body]
  termDecoder = tCon2' sModule Module

sBHole, sBody :: Symbol
sBHole = newSymbol "BHole"
sBody  = newSymbol "Body"

instance IsTerm Body where
  toTerm body = case body of
    BHole      -> TCon sBHole []
    Body decls -> TCon sBody  [toTerm decls]
  termDecoder = tCon0' sBHole BHole <|> tCon1' sBody Body

sDHole, sDEmpty, sDFunBinds, sDPatBind :: Symbol
sDHole     = newSymbol "DHole"
sDEmpty    = newSymbol "DEmpty"
sDFunBinds = newSymbol "DFunBinds"
sDPatBind  = newSymbol "DPatBind"

instance IsTerm Decl where
  toTerm decl = case decl of
    DHole id           -> TCon sDHole     [toTerm id]
    DEmpty             -> TCon sDEmpty    []
    DFunBinds funbinds -> TCon sDFunBinds [toTerm funbinds]
    DPatBind pat rhs   -> TCon sDPatBind  [toTerm pat, toTerm rhs]
  termDecoder =  tCon1' sDHole DHole
             <|> tCon0' sDEmpty DEmpty
             <|> tCon1' sDFunBinds DFunBinds
             <|> tCon2' sDPatBind DPatBind 

sHole, sFeedback, sMustUse, sEta, sRefactor, sCase, sCon, sIf :: Symbol
sInfixApp, sLambda, sLet, sLit, sApp, sParen, sTuple, sVar, sEnum :: Symbol
sList, sNeg :: Symbol
sHole     = newSymbol "Hole"    
sFeedback = newSymbol "Feedback"
sMustUse  = newSymbol "MustUse" 
sEta      = newSymbol "Eta"     
sRefactor = newSymbol "Refactor"
sCase     = newSymbol "Case"    
sCon      = newSymbol "Con"     
sIf       = newSymbol "If"      
sInfixApp = newSymbol "InfixApp"
sLambda   = newSymbol "Lambda"  
sLet      = newSymbol "Let"     
sLit      = newSymbol "Lit"     
sApp      = newSymbol "App"     
sParen    = newSymbol "Paren"   
sTuple    = newSymbol "Tuple"   
sVar      = newSymbol "Var"     
sEnum     = newSymbol "Enum"    
sList     = newSymbol "List"    
sNeg      = newSymbol "Neg"     

instance IsTerm Expr where
  toTerm expr = case expr of
    Hole id           -> TCon sHole     [toTerm id]
    Feedback msg expr -> TCon sFeedback [toTerm msg, toTerm expr]
    MustUse  expr     -> TCon sMustUse  [toTerm expr]
    Eta n expr        -> TCon sEta      [toTerm n, toTerm expr]
    Refactor e alts   -> TCon sRefactor [toTerm e, toTerm alts]
    Case expr alts    -> TCon sCase     [toTerm expr, toTerm alts]
    Con name          -> TCon sCon      [toTerm name]
    If c t e          -> TCon sIf       [toTerm c, toTerm t, toTerm e]
    InfixApp l op r   -> TCon sInfixApp [toTerm l, toTerm op, toTerm r]
    Lambda pats expr  -> TCon sLambda   [toTerm pats, toTerm expr]
    Let decls expr    -> TCon sLet      [toTerm decls, toTerm expr]
    Lit lit           -> TCon sLit      [toTerm lit]
    App fun args      -> TCon sApp      [toTerm fun, toTerm args]
    Paren expr        -> TCon sParen    [toTerm expr]
    Tuple exprs       -> TCon sTuple    [toTerm exprs]
    Var name          -> TCon sVar      [toTerm name]
    Enum from t to    -> TCon sEnum     [toTerm from, toTerm t, toTerm to]
    List exprs        -> TCon sList     [toTerm exprs]
    Neg expr          -> TCon sNeg      [toTerm expr]
  termDecoder =  tCon1' sHole Hole
             <|> tCon2' sFeedback Feedback
             <|> tCon1' sMustUse MustUse 
             <|> tCon2' sEta Eta 
             <|> tCon2' sRefactor Refactor 
             <|> tCon2' sCase Case 
             <|> tCon1' sCon Con
             <|> tCon3' sIf If
             <|> tCon3' sInfixApp InfixApp
             <|> tCon2' sLambda Lambda
             <|> tCon2' sLet Let
             <|> tCon1' sLit Lit
             <|> tCon2' sApp App 
             <|> tCon1' sParen Paren
             <|> tCon1' sTuple Tuple
             <|> tCon1' sVar Var
             <|> tCon3' sEnum Enum
             <|> tCon1' sList List
             <|> tCon1' sNeg Neg

sNoExpr, sJustExpr :: Symbol
sNoExpr   = newSymbol "NoExpr"
sJustExpr = newSymbol "JustExpr"

instance IsTerm MaybeExpr where
  toTerm mexpr = case mexpr of
    NoExpr        -> TCon sNoExpr  []
    JustExpr expr -> TCon sJustExpr [toTerm expr]
  termDecoder = tCon0' sNoExpr NoExpr <|> tCon1' sJustExpr JustExpr

sRefactorChoice :: Symbol
sRefactorChoice = newSymbol "RefactorChoice"

instance IsTerm RefactorChoice where
  toTerm (RefactorChoice r e) = TCon sRefactorChoice [toTerm r, toTerm e]
  termDecoder = tCon2' sRefactorChoice RefactorChoice

sAHole, sAlt, sAltEmpty :: Symbol
sAHole    = newSymbol "AHole"
sAlt      = newSymbol "Alt"
sAltEmpty = newSymbol "AltEmpty"

instance IsTerm Alt where
  toTerm alt = case alt of
    AHole id       -> TCon sAHole    [toTerm id]
    Alt fb pat rhs -> TCon sAlt      [toTerm fb, toTerm pat, toTerm rhs]
    AltEmpty       -> TCon sAltEmpty []
  termDecoder = tCon1' sAHole AHole <|> tCon3' sAlt Alt <|> tCon0' sAltEmpty AltEmpty

sFBHole, sFunBind :: Symbol
sFBHole  = newSymbol "FBHole"
sFunBind = newSymbol "FunBind"

instance IsTerm FunBind where
  toTerm funbind = case funbind of
    FBHole id -> TCon sFBHole [toTerm id]
    FunBind fb name pats rhs -> 
      TCon sFunBind [toTerm fb, toTerm name, toTerm pats, toTerm rhs]
  termDecoder =  tCon1' sFBHole FBHole 
             <|> tCon sFunBind (FunBind <$> termDecoder <*> termDecoder <*> termDecoder <*> termDecoder)

sGExpr :: Symbol
sGExpr = newSymbol "GExpr"

instance IsTerm GuardedExpr where
  toTerm (GExpr guard expr) = TCon sGExpr [toTerm guard, toTerm expr]
  termDecoder = tCon2' sGExpr GExpr

sLChar, sLFloat, sLInt, sLString :: Symbol
sLChar   = newSymbol "LChar"
sLFloat  = newSymbol "LFloat"
sLInt    = newSymbol "LInt"
sLString = newSymbol "LString"

instance IsTerm Literal where
  toTerm lit = case lit of
    LChar val   -> TCon sLChar   [toTerm val]
    LFloat val  -> TCon sLFloat  [toTerm (realToFrac val :: Double)]
    LInt val    -> TCon sLInt    [toTerm val]
    LString val -> TCon sLString [toTerm val]
  termDecoder =  tCon1' sLChar LChar
             <|> tCon1' sLFloat LFloat
             <|> tCon1' sLInt LInt
             <|> tCon1' sLString LString

sIdent, sOperator, sSpecial :: Symbol
sIdent    = newSymbol "Ident"
sOperator = newSymbol "Operator"
sSpecial  = newSymbol "Special"

instance IsTerm Name where
  toTerm name = case name of
    Ident name    -> TCon sIdent    [toTerm name]
    Operator name -> TCon sOperator [toTerm name]
    Special name  -> TCon sSpecial  [toTerm name]
  termDecoder =  tCon1' sIdent Ident
             <|> tCon1' sOperator Operator
             <|> tCon1' sSpecial Special

sNoName, sJustName :: Symbol
sNoName   = newSymbol "NoName"
sJustName = newSymbol "JustName"

instance IsTerm MaybeName where
  toTerm mname = case mname of
    NoName        -> TCon sNoName []
    JustName name -> TCon sJustName [toTerm name]
  termDecoder = tCon0' sNoName NoName <|> tCon1' sJustName JustName

sPHole, sPMultipleHole, sPCon, sPInfixCon, sPList, sPLit, sPParen :: Symbol
sPTuple, sPVar, sPAs, sPWildcard :: Symbol
sPHole         = newSymbol "PHole"     
sPMultipleHole = newSymbol "PMultipleHole"
sPCon          = newSymbol "PCon"     
sPInfixCon     = newSymbol "PInfixCon"
sPList         = newSymbol "PList"    
sPLit          = newSymbol "PLit"     
sPParen        = newSymbol "PParen"   
sPTuple        = newSymbol "PTuple"   
sPVar          = newSymbol "PVar"     
sPAs           = newSymbol "PAs"      
sPWildcard     = newSymbol "PWildcard"

instance IsTerm Pat where
  toTerm pat = case pat of
    PHole id           -> TCon sPHole     [toTerm id]
    PMultipleHole id   -> TCon sPMultipleHole [toTerm id]
    PCon name pats     -> TCon sPCon      [toTerm name, toTerm pats]
    PInfixCon l name r -> TCon sPInfixCon [toTerm l, toTerm name, toTerm r]
    PList pats         -> TCon sPList     [toTerm pats]
    PLit lit           -> TCon sPLit      [toTerm lit]
    PParen pat         -> TCon sPParen    [toTerm pat]
    PTuple pats        -> TCon sPTuple    [toTerm pats]
    PVar name          -> TCon sPVar      [toTerm name]
    PAs name pat       -> TCon sPAs       [toTerm name, toTerm pat]
    PWildcard          -> TCon sPWildcard []
  termDecoder =  tCon1' sPHole PHole
             <|> tCon1' sPMultipleHole PHole
             <|> tCon2' sPCon PCon
             <|> tCon3' sPInfixCon PInfixCon
             <|> tCon1' sPList PList
             <|> tCon1' sPLit PLit
             <|> tCon1' sPParen PParen
             <|> tCon1' sPTuple PTuple
             <|> tCon1' sPVar PVar
             <|> tCon2' sPAs PAs
             <|> tCon0' sPWildcard PWildcard

sRhs, sGRhs :: Symbol
sRhs  = newSymbol "Rhs"
sGRhs = newSymbol "GRhs"

instance IsTerm Rhs where
  toTerm rhs = case rhs of
    Rhs expr where_    -> TCon sRhs  [toTerm expr, toTerm where_]
    GRhs gexprs where_ -> TCon sGRhs [toTerm gexprs, toTerm where_]
  termDecoder = tCon2' sRhs Rhs <|> tCon2' sGRhs GRhs

instance Reference Name

-- | A convenient shorthand notation for Biplate constraints
class ( Biplate b Module, Biplate b Body, Biplate b Decl, Biplate b Expr
      , Biplate b FunBind, Biplate b Alt, Biplate b Pat
      , Biplate b Rhs, Typeable b ) 
      => BiplateFor b
instance BiplateFor Module      where
instance BiplateFor Decl        where
instance BiplateFor Rhs         where
instance BiplateFor Pat         where
instance BiplateFor Expr        where  
instance BiplateFor GuardedExpr where  

-- GENERATED START
-- GENERATED STOP
