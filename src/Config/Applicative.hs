{- |
This module provides a unified method of configuring a program, being able to
read settings from a ini-format file, from environment variables, and from
the command line in a predictable and uniform manner, with room for
customisation.
-}

module Config.Applicative
  (
    -- * 'Option'
    Opt.Option, Opt.option, Opt.optionMaybe, Opt.optionMany, Opt.optionSome
  , Opt.optionMap, Opt.commands, Opt.check, Opt.withIO
    -- * Modifiers
  , Inf.Info, Inf.name, Inf.long, Inf.short, Inf.envVar, Inf.help, Inf.metavar
  , Inf.value, Inf.sample
    -- * Reader and Pretty-Printing
  , Rdr.Reader, Rdr.str, Rdr.text, Rdr.auto, Rdr.boolean, Rdr.enum, Rdr.enumCI
  , Rdr.nonNegative, Rdr.positive, Rdr.password, Rdr.maybeReader, Rdr.eitherReader
  , Rdr.lookupReader
    -- * Parsing
  , Psr.ParseError(..), Psr.mkParser
    -- * Example Generation
  , Exa.genExample
    -- * Other
  , Typ.IniVariable(..), Typ.Sample(..), Typ.Domain(..)
  ) where

import qualified Config.Applicative.Example as Exa
import qualified Config.Applicative.Info    as Inf
import qualified Config.Applicative.Option  as Opt
import qualified Config.Applicative.Parse   as Psr
import qualified Config.Applicative.Reader  as Rdr
import qualified Config.Applicative.Types   as Typ
