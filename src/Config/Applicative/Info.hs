module Config.Applicative.Info
  ( name, long, short, envVar, help, metavar, value, example, autoLong
  ) where

import Config.Applicative.Types (Info(..))

import Control.Applicative ((<|>))
import Data.Char           (isAlphaNum, toUpper)
import Text.Printf         (printf)

import qualified Data.Set as Set

-- | Build a minimal 'Info' with section and variable names.
name :: String -> String -> Info a
name section variable =
  Info section variable [] Set.empty envVarNm Nothing "ARG" Nothing Nothing
  where
    envVarNm prefix =
      map (\x -> if isAlphaNum x || x == '_' then x else '_')
      $ printf "%s_%s_%s" prefix section (map toUpper variable)

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
metavar v i = i{ optMetavar = v }

-- | Set the default value of this option.  Used as an example value in
-- generated ini files.
value :: a -> Info a -> Info a
value x i = i{ optValue = Just x, optExample = optExample i <|> Just x }

-- | Set the example value to use in generated ini files without giving the option a
-- default.
example :: a -> Info a -> Info a
example x i = i{ optExample = Just x }

-- | Derive the default long command line option from the section and variable
-- names.
autoLong :: Info a -> Info a
autoLong i = i{ optLongs = optLongs i ++ [s ++ "." ++ v]  }
  where
    s = optSection i
    v = optVariable i
