{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config.Applicative.Types
  ( Domain(..), Metavar(..), Example(..)
  , Info(..), Reader(..), F(..)
  , Option(..)
  , Validation(..)
  ) where

import Control.Applicative.Free (Ap)
import Data.List.NonEmpty       (NonEmpty)
import Data.Map                 (Map)
import Data.Set                 (Set)

newtype Domain    = Domain (Maybe [String]) deriving (Eq, Ord, Show)
newtype Metavar   = Metavar String          deriving (Eq, Ord, Show)
newtype Example a = Example (Maybe a)       deriving (Eq, Ord, Show)

data Info a = Info
  { optSection  :: String
  , optVariable :: String
  , optLongs    :: [String]
  , optShorts   :: Set Char
  , optEnvVar   :: String -> String
  , optHelp     :: Maybe String
  , optMetavar  :: String
  , optValue    :: Maybe a
  , optExample  :: Maybe a
  } deriving Functor

-- | A 'Reader' actually represents both a parser and pretty-printer.
data Reader a
  = Reader (String -> Either String a) (a -> String) (Maybe [a])

-- | This module is built on the free applicative functor, or
-- 'Control.Applicative.Free.Ap' from the package @free@.
--
-- It takes a \"carrier\" functor which defines the interesting parts of the
-- structure, and the free applicative functor provides a way of combining the
-- carriers.
data F a where
  One      :: Reader a -> Info a -> F a
  Optional :: Reader a -> Info a -> F (Maybe a)
  Many     :: Reader a -> Info a -> F [a]
  Some     :: Reader a -> Info a -> F (NonEmpty a)
  Map      :: Reader a -> Info (String, a) -> F (Map String a)
  Commands :: Info String -> [(String, (Maybe String, Ap F a))] -> F a
  WithIO   :: String -> (a -> IO (Either String b)) -> Ap F a -> F b

-- | Wrap @Ap F a@ in an opaque @newtype@.
newtype Option a = Option{ getOption :: Ap F a }
  deriving (Functor, Applicative)


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
