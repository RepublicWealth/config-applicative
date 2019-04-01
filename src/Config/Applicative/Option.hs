{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config.Applicative.Option where

import Config.Applicative.Info   (Info(..), autoLong)
import Config.Applicative.Reader (Reader(..))

import Control.Applicative.Free (Ap, liftAp)
import Data.List.NonEmpty       (NonEmpty)
import Data.Map.Strict          (Map)

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

-- | A configuration option to parse.  Using 'Config.Applicative.Parse.mkParser'
-- config data can be read from a number of sources in a uniform manner.
-- 'Option's can be combined using the usual 'Applicative' combinators.
newtype Option a = Option{ getOption :: Ap F a }
  deriving (Functor, Applicative)

-- | Parse an item.
option :: Reader a -> Info a -> Option a
option rdr i = Option $ liftAp $ One rdr (autoLong i)

-- | Parse an item, optionally.
optionMaybe :: Reader a -> Info a -> Option (Maybe a)
optionMaybe rdr i = Option $ liftAp $ Optional rdr (autoLong i)

-- | Parse any number of items as a list.
optionMany :: Reader a -> Info a -> Option [a]
optionMany rdr i = Option $ liftAp $ Many rdr (autoLong i)

-- | Parse at least one item as a 'NonEmpty' list.
optionSome :: Reader a -> Info a -> Option (NonEmpty a)
optionSome rdr i = Option $ liftAp $ Some rdr (autoLong i)

-- | Parse key-value items into a 'Map'.
optionMap :: Reader a -> Info (String, a) -> Option (Map String a)
optionMap rdr i = Option $ liftAp $ Map rdr (autoLong i)

-- | Switch on a list of mutually exclusive options.
--
-- The 'String' in the first place defines the recognised option in ini files
-- and environment variables, while the 'Maybe' 'String' in the second tuple
-- place allows for overriding the command line syntax.
commands :: Info String -> [(String, Maybe String, Option a)] -> Option a
commands i cmds = Option $ liftAp $ Commands i $ map f cmds
  where
    f (a, b, c) = (a, (b, getOption c))

-- | Transform an 'Option' to check that some property holds on the value
-- parsed.  The String is used when the check fails to use in the error message.
check :: String -> (a -> Either String b) -> Option a -> Option b
check nm f (Option m) = Option $ liftAp $ WithIO nm (pure . f) m

-- | Transform an 'Option' to run some IO action after parsing a value.  The
-- action can fail nicely using a Left in the Either return value.
withIO :: String -> (a -> IO (Either String b)) -> Option a -> Option b
withIO nm f (Option m) = Option $ liftAp $ WithIO nm f m
