{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -fno-warn-orphans #-}
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
module Helium.Utility.Instances () where

import Helium.StaticAnalysis.Messages.Messages ()

-- import Helium.Syntax.UHA_Range
import Helium.Syntax.UHA_Syntax
    ( Alternative (..)
    , AnnotatedType (..)
    , Body (..)
    , Constructor (..)
    , ContextItem (..)
    , Declaration (..)
    , Export (..)
    , Expression (..)
    , FieldDeclaration (..)
    , Fixity (..)
    , FunctionBinding (..)
    , GuardedExpression (..)
    , Import (..)
    , ImportDeclaration (..)
    , ImportSpecification (..)
    , LeftHandSide (..)
    , Literal (..)
    , MaybeDeclarations (..)
    , MaybeExports (..)
    , MaybeExpression (..)
    , MaybeImportSpecification (..)
    , MaybeInt (..)
    , MaybeName (..)
    , MaybeNames (..)
    , Module (..)
    , Pattern (..)
    , Qualifier (..)
    , RecordExpressionBinding (..)
    , RecordPatternBinding (..)
    , RightHandSide (..)
    , SimpleType (..)
    , Statement (..)
    , Type (..)
    )

--------------------------------------------------------------------
-- Derived  stock instances

deriving stock instance Show Module

deriving stock instance Show Body

deriving stock instance Show MaybeName

deriving stock instance Show MaybeNames

deriving stock instance Show MaybeExports

deriving stock instance Show Declaration

deriving stock instance Show ImportDeclaration

deriving stock instance Show Export

deriving stock instance Show Type

deriving stock instance Show RightHandSide

deriving stock instance Show Pattern

deriving stock instance Show Constructor

deriving stock instance Show FunctionBinding

deriving stock instance Show MaybeInt

deriving stock instance Show Fixity

deriving stock instance Show MaybeDeclarations

deriving stock instance Show SimpleType

deriving stock instance Show ContextItem

deriving stock instance Show MaybeImportSpecification

deriving stock instance Show Expression

deriving stock instance Show RecordPatternBinding

deriving stock instance Show Literal

deriving stock instance Show GuardedExpression

deriving stock instance Show FieldDeclaration

deriving stock instance Show AnnotatedType

deriving stock instance Show LeftHandSide

deriving stock instance Show ImportSpecification

deriving stock instance Show RecordExpressionBinding

deriving stock instance Show MaybeExpression

deriving stock instance Show Statement

deriving stock instance Show Qualifier

deriving stock instance Show Alternative

deriving stock instance Show Import

deriving stock instance Eq Module

deriving stock instance Eq Body

deriving stock instance Eq MaybeName

deriving stock instance Eq MaybeNames

deriving stock instance Eq MaybeExports

deriving stock instance Eq ImportDeclaration

deriving stock instance Eq Export

deriving stock instance Eq Expression

deriving stock instance Eq Type

deriving stock instance Eq RecordExpressionBinding

deriving stock instance Eq Literal

deriving stock instance Eq Declaration

deriving stock instance Eq Pattern

deriving stock instance Eq MaybeExpression

deriving stock instance Eq Statement

deriving stock instance Eq Qualifier

deriving stock instance Eq Alternative

deriving stock instance Eq RightHandSide

deriving stock instance Eq Constructor

deriving stock instance Eq FunctionBinding

deriving stock instance Eq MaybeInt

deriving stock instance Eq Fixity

deriving stock instance Eq MaybeDeclarations

deriving stock instance Eq SimpleType

deriving stock instance Eq FieldDeclaration

deriving stock instance Eq AnnotatedType

deriving stock instance Eq LeftHandSide

deriving stock instance Eq ContextItem

deriving stock instance Eq RecordPatternBinding

deriving stock instance Eq GuardedExpression

deriving stock instance Eq MaybeImportSpecification

deriving stock instance Eq ImportSpecification

deriving stock instance Eq Import
