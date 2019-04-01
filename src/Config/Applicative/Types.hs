{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}

module Config.Applicative.Types
  ( IniVariable(..), ivSection, ivVariable
  , Domain(..), Metavar(..), Sample(..)
  , Validation(..)
  ) where

import Control.Applicative ((<|>))

-- | Ini-file section and variable names.
data IniVariable = IniVariable String String
  deriving (Eq, Ord, Show)

ivSection :: IniVariable -> String
ivSection (IniVariable s _) = s

ivVariable :: IniVariable -> String
ivVariable (IniVariable _ v) = v

-- | The domain of values of a variable, if known.
newtype Domain = Domain (Maybe [String]) deriving (Eq, Ord, Show)

-- | The metavar string to use in help output and example files.
newtype Metavar = Metavar String deriving (Eq, Ord, Show)

-- | An optional sample value of an configuration option.
newtype Sample a = Sample (Maybe a) deriving (Eq, Ord, Show, Functor)

instance Semigroup (Sample a) where
  Sample a <> Sample b = Sample (a <|> b)

instance Monoid (Sample a) where
  mempty  = Sample Nothing
  mappend = (<>)


--
-- Validation
--

data Validation e a
  = Success a
  | Failure e

instance Functor (Validation e) where
  fmap f (Success x) = Success (f x)
  fmap _ (Failure e) = Failure e

instance Semigroup e => Applicative (Validation e) where
  pure = Success
  Success f  <*> Success x  = Success (f x)
  Success _  <*> Failure xe = Failure xe
  Failure fe <*> Success _  = Failure fe
  Failure fe <*> Failure xe = Failure (fe <> xe)
