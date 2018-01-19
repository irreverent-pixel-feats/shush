{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Shush.Data.Environment
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Shush.Data.Environment (
  -- * Types
    ChildEnvironment
  -- * Operators
  , (.->)
  , (.?>)
  , (.::)
  -- * Functions
  , buildChildEnvironmentFromParent
  , importEnvVar
  , importEnvVarOptional
  ) where

import qualified Ultra.Data.Text as T

import qualified Data.HashMap.Strict as M
import Data.Validation (AccValidation(..))

import Preamble

infixr 4 .->
infixr 4 .?>
infixr 4 .::

-- |
-- An environment variable to define the environment of a child process
-- For security reasons it's best to be explicit about what environment
-- variables you want set for child processes and where you want them to
-- come from.
--
-- It is modelled as a Transformation of the Parent Environment to the
-- child environment
--
-- Constructors are not exported so that as a practice you can't
-- just import the parent environment wholesale.
-- (Not conveniently, at least)
--
-- use the `.->`, `.?>`, `.::`, `importEnvVar` and `importVarOptional`
-- smart constructors instead.
--
-- Combine them with the Monoid instance.
--
newtype ChildEnvironment = ChildEnvironment {
    _buildChildEnvironmentFromParent :: M.HashMap T.Text T.Text -> AccValidation (NonEmpty T.Text) [(T.Text, T.Text)]
  }

buildChildEnvironmentFromParent
  :: ChildEnvironment
  -> M.HashMap T.Text T.Text
  -> AccValidation (NonEmpty T.Text) [(T.Text, T.Text)]
buildChildEnvironmentFromParent = _buildChildEnvironmentFromParent

instance Monoid ChildEnvironment where
--mempty :: a
  mempty = ChildEnvironment . const . pure $ []

--mappend :: a -> a -> a
  mappend (ChildEnvironment f) (ChildEnvironment g) = ChildEnvironment $ \parentEnv ->
    (<>) <$> f parentEnv <*> g parentEnv

-- | A var from the Parent environment is mapped to a var in the child environment
-- of a distinct name
-- It fails if the variable is not set in the parent environment
(.->) :: T.Text -> T.Text -> ChildEnvironment
parName .-> childName = ChildEnvironment $
  maybe (AccFailure (pure parName)) (pure . pure . ((,) childName)) . M.lookup parName

-- | A var from the Parent environment is mapped to a var in the child environment
-- of a distinct name
-- If the var is not set in the parent environment, it is not added to the child environment
(.?>) :: T.Text -> T.Text -> ChildEnvironment
parName .?> childName = ChildEnvironment $
  maybe (pure []) (pure . pure . ((,) childName)) . M.lookup parName

-- | Adds a new environment variable to the child environment
(.::) :: T.Text -> T.Text -> ChildEnvironment
name .:: value = ChildEnvironment $ \_ ->
  pure . pure $ (name, value)

-- |
-- looks for the specified env var in the parent environment and
-- creates the same variable in the child environment
-- fails if the environment variable is missing
--
importEnvVar :: T.Text -> ChildEnvironment
importEnvVar name = name .-> name

-- |
-- looks for the specified env var in the parent environment and
-- creates the same variable in the child environment
-- If the parent env variable doesnt exist,
-- the operation passes, but nothing is set in the child environment
--
importEnvVarOptional :: T.Text -> ChildEnvironment
importEnvVarOptional name = name .?> name
