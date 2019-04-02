{-# LANGUAGE FlexibleContexts #-}

module Config.Applicative.Driver
  ( ParseSetup(..), ConfigParser, prepare
  ) where

import Control.Applicative  (empty, many, optional, (<**>), (<|>))
import Control.Monad.Except (MonadError, MonadIO, throwError)

import qualified Config.Applicative         as Cfg
import qualified Config.Applicative.Example as Cfg
import qualified Config.Applicative.Parse   as Cfg

import qualified Data.Ini            as Ini
import qualified Options.Applicative as Opt

type ConfigParser a
  = Ini.Ini
    -> [(String, String)]
    -> Opt.ParserInfo (IO (Either [Cfg.ParseError] a))

data ParseSetup a
  = GetConfig [FilePath] [String] (ConfigParser a)
  | DumpIni   [FilePath] [String] (ConfigParser Ini.Ini)
  | PrintExample String

extraOpts :: Maybe FilePath -> Opt.Parser (Maybe FilePath, Bool, Bool)
extraOpts configPathMay = (,,)
  <$> (maybe empty (pure . Just) configPathMay
       <|> optional (Opt.option (Opt.str :: Opt.ReadM String) (
             Opt.short 'c'
             <> Opt.long "config"
             <> Opt.help "Supply a path to the configuration .ini file.")))
  <*> Opt.switch (
        Opt.long "config-example"
        <> Opt.help "Produce a sample configuration file on stdout and exit.")
  <*> Opt.switch (
        Opt.long "config-dump"
        <> Opt.help "Dump an ini file containing the parsed configuration.")

configParserInfo
  :: String
  -> Cfg.Option a
  -> Ini.Ini
  -> [(String, String)]
  -> Opt.ParserInfo (IO (Either [Cfg.ParseError] (a, Ini.Ini)))
configParserInfo env_prefix defn ini env = Opt.info (psr <**> Opt.helper) $ mconcat opts
  where
    psr = extraOpts Nothing *> Cfg.mkParser env_prefix ini env defn
    opts = [Opt.fullDesc]

prepare
  :: (MonadIO m, MonadError String m)
  => String
  -> String
  -> Cfg.Option a
  -> Maybe FilePath
  -> [String]
  -> m (ParseSetup a)
prepare prog_name env_prefix defn configPathMay args = do
  let psr = (,) <$> extraOpts configPathMay
                <*> many (Opt.argument Opt.str mempty)
  let info  = Opt.info psr Opt.forwardOptions
  let prefs = Opt.prefs mempty
  r <- case Opt.execParserPure prefs info args of
    Opt.Success x                     -> pure x
    Opt.Failure (Opt.ParserFailure f) -> throwError (show h) where (h, _, _) = f prog_name
    Opt.CompletionInvoked _           -> error "completion"
  let defaultPaths   = []
  let goCfg  ini env = fmap (fmap fst) <$> configParserInfo env_prefix defn ini env
  let goDump ini env = fmap (fmap snd) <$> configParserInfo env_prefix defn ini env
  case r of
    ((    _ini_path,  True, _dump),     _) -> pure (PrintExample (Cfg.genExample env_prefix defn))
    ((      Nothing, False,  True), args') -> pure (DumpIni   defaultPaths args' goDump)
    ((      Nothing, False, False), args') -> pure (GetConfig defaultPaths args' goCfg)
    ((Just ini_path, False,  True), args') -> pure (DumpIni   [ini_path]   args' goDump)
    ((Just ini_path, False, False), args') -> pure (GetConfig [ini_path]   args' goCfg)
