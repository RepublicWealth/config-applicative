{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}

module Config.Applicative.Info
  ( Info(..), optSection, optVariable, name, long, short, envVar, help
  , metavar, value, sample, autoLong
  ) where

import Config.Applicative.Types
  (IniVariable(..), Metavar(..), Sample(..), ivSection, ivVariable)

import Data.Char   (isAlphaNum, toUpper)
import Data.Set    (Set)
import Text.Printf (printf)


import qualified Data.Set as Set

data Info a = Info
  { optIniVariable :: IniVariable
  , optLongs       :: [String]
  , optShorts      :: Set Char
  , optEnvVar      :: String -> String
  , optHelp        :: Maybe String
  , optMetavar     :: Metavar
  , optValue       :: Maybe a
  , optSample      :: Sample a
  } deriving Functor

optSection, optVariable :: Info a -> String
optSection  = ivSection  . optIniVariable
optVariable = ivVariable . optIniVariable

-- | Build a minimal 'Info' with section and variable names.
name :: String -> String -> Info a
name section variable =
  Info (IniVariable section variable) [] Set.empty envVarNm help' (Metavar "ARG") value' sample'
  where
    envVarNm prefix =
      map (\x -> if isAlphaNum x || x == '_' then x else '_')
      $ printf "%s_%s_%s" prefix section (map toUpper variable)
    help'   = Nothing
    value'  = Nothing
    sample' = Sample Nothing

-- | Add an additional long command line option.
long :: String -> Info a -> Info a
long nm i = i{ optLongs = optLongs i ++ [nm] }

-- | Add an additional short command line option.
short :: Char -> Info a -> Info a
short c i = i{ optShorts = Set.insert c (optShorts i) }

-- | Use a different environment variable from the default.
envVar :: String -> Info a -> Info a
envVar nm i = i{ optEnvVar = const nm }

-- | Set the help string, reproduced in command line @--help@ and when
-- generating an ini file.
help :: String -> Info a  -> Info a
help h i = i{ optHelp = Just h }

-- | Set the metavar to use in command line @--help@ and in the generated ini
-- files.
metavar :: String -> Info a -> Info a
metavar v i = i{ optMetavar = Metavar v }

-- | Set the default value of this option.  Used as an example value in
-- generated ini files.
value :: a -> Info a -> Info a
value x i = i{ optValue = Just x, optSample = optSample i <> Sample (Just x)}

-- | Set the sample value to use in generated ini files without giving the option a
-- default.
sample :: a -> Info a -> Info a
sample x i = i{ optSample = Sample (Just x) }

-- | Derive the default long command line option from the section and variable
-- names.
autoLong :: Info a -> Info a
autoLong i = i{ optLongs = optLongs i ++ [s ++ "." ++ v]  }
  where
    IniVariable s v = optIniVariable i
