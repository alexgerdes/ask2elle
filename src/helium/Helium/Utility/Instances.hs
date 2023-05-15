{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
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
module Helium.Utility.Instances where

import Helium.StaticAnalysis.Messages.Messages

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
-- Derived instances

deriving instance Show Module

deriving instance Show Body

deriving instance Show MaybeName

deriving instance Show MaybeNames

deriving instance Show MaybeExports

deriving instance Show Declaration

deriving instance Show ImportDeclaration

deriving instance Show Export

deriving instance Show Type

deriving instance Show RightHandSide

deriving instance Show Pattern

deriving instance Show Constructor

deriving instance Show FunctionBinding

deriving instance Show MaybeInt

deriving instance Show Fixity

deriving instance Show MaybeDeclarations

deriving instance Show SimpleType

deriving instance Show ContextItem

deriving instance Show MaybeImportSpecification

deriving instance Show Expression

deriving instance Show RecordPatternBinding

deriving instance Show Literal

deriving instance Show GuardedExpression

deriving instance Show FieldDeclaration

deriving instance Show AnnotatedType

deriving instance Show LeftHandSide

deriving instance Show ImportSpecification

deriving instance Show RecordExpressionBinding

deriving instance Show MaybeExpression

deriving instance Show Statement

deriving instance Show Qualifier

deriving instance Show Alternative

deriving instance Show Import

deriving instance Eq Module

deriving instance Eq Body

deriving instance Eq MaybeName

deriving instance Eq MaybeNames

deriving instance Eq MaybeExports

deriving instance Eq ImportDeclaration

deriving instance Eq Export

deriving instance Eq Expression

deriving instance Eq Type

deriving instance Eq RecordExpressionBinding

deriving instance Eq Literal

deriving instance Eq Declaration

deriving instance Eq Pattern

deriving instance Eq MaybeExpression

deriving instance Eq Statement

deriving instance Eq Qualifier

deriving instance Eq Alternative

deriving instance Eq RightHandSide

deriving instance Eq Constructor

deriving instance Eq FunctionBinding

deriving instance Eq MaybeInt

deriving instance Eq Fixity

deriving instance Eq MaybeDeclarations

deriving instance Eq SimpleType

deriving instance Eq FieldDeclaration

deriving instance Eq AnnotatedType

deriving instance Eq LeftHandSide

deriving instance Eq ContextItem

deriving instance Eq RecordPatternBinding

deriving instance Eq GuardedExpression

deriving instance Eq MaybeImportSpecification

deriving instance Eq ImportSpecification

deriving instance Eq Import
