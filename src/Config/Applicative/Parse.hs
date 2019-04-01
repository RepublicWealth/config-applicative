{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

--
-- PARSING CONFIGURATION
--

-- An 'Option' is a chain of descriptions of options that we'd like to parse.
-- This is the parsing interpretation of an 'Option', what is built and returned
-- for the user to run.
--
-- The 'Compose' uses are noise and can be ignored.  The structure is:
--
-- Opt.Parser
-- -> IO
--    -> Product (pair)
--       -> Const [Ini]
--       -> Validation [ParseError]
--
-- The command line parser is actually a result of parsing the 'Ini' file and
-- the environment variables, so if a value is found in either of those for a
-- given variable the generated command line parser actually makes that option
-- optional, even if it's not optional to return a value, although it won't be
-- optional if no value was found in the 'Ini' file or the environment.
--
-- Assuming command line parsing succeeds, an IO action will be returned which
-- allows 'WithIO' actions to be run after parsing from the sources.
--
-- A parsing failure is represented as a list of errors in a 'Validation'.
-- 'Validation's do not have a 'Monad' instance, only 'Applicative' (and
-- 'Functor') so as they are combined they collect all the errors together,
-- meaning we can return all the errors found from parsing everything provided,
-- not just failing with the first error encountered.

module Config.Applicative.Parse
  ( mkParser, ParseError(..)
  ) where

import Config.Applicative.Info   (Info(..), optSection, optVariable)
import Config.Applicative.Option (F(..), Option(..))
import Config.Applicative.Reader (Reader(..), lookupReader, str)
import Config.Applicative.Types
  (Domain(..), IniVariable(..), Metavar(..), Sample(..), Validation(..))

import Control.Applicative      (empty, many, some, (<**>), (<|>))
import Control.Applicative.Free (Ap, runAp)
import Data.Bifunctor           (bimap)
import Data.Foldable            (find, fold, toList)
import Data.Functor.Compose     (Compose(Compose, getCompose))
import Data.Functor.Const       (Const(Const))
import Data.Functor.Identity    (Identity(Identity, runIdentity))
import Data.Functor.Product     (Product(Pair))
import Data.Ini                 (Ini)
import Data.List                (uncons, union)
import Data.Map.Strict          (Map)
import Data.Maybe               (fromMaybe, mapMaybe, maybeToList)
import Data.Text                (Text)
import Data.Traversable         (for)
import Text.Printf              (printf)

import qualified Data.HashMap.Strict as HM
import qualified Data.Ini            as Ini
import qualified Data.List.NonEmpty  as NE
import qualified Data.Map            as Map
import qualified Data.Text           as Text
import qualified Options.Applicative as Opt

data ParseError
    -- | Error location (section+variable), message, possibly an sample value
    -- that could have been used, and the domain of values if known.
  = ParseError IniVariable String (Sample String) Domain
    -- | A 'withIO' returned a 'Left' value.  First field is the 'withIO' label
    -- and the second is the message returned in the 'Left'.
  | CheckError String String
    deriving (Eq, Ord, Show)

newtype M a
  = M{ unM :: Compose
                (Compose Opt.Parser IO)
                (Product
                   (Const [Ini])
                   (Validation [ParseError])
                ) a
     }
  deriving (Functor, Applicative)

-- | Wrap in an 'M'.
cc :: Opt.Parser (IO ([Ini], Validation [ParseError] a)) -> M a
cc = M . Compose . Compose . fmap (fmap (\(inis, val) -> Pair (Const inis) val))

-- | Unwrap from an 'M'.
uu :: M a -> Opt.Parser (IO ([Ini], Validation [ParseError] a))
uu = fmap (fmap (\(Pair (Const inis) val) -> (inis, val))) . getCompose . getCompose . unM

-- | Lift a command line parser for an 'a' into an 'M' 'a'.
liftPsr :: Opt.Parser a -> M a
liftPsr psr = cc ((\x -> pure (mempty, pure x)) <$> psr)

-- | Lift a list of failures into an 'M' 'a'.
liftErrs :: [ParseError] -> M a
liftErrs es = cc (pure (pure (mempty, Failure es)))

mkIni :: (Text -> Text) -> IniVariable -> String -> Ini
mkIni varF (IniVariable section variable) value =
  Ini.Ini (HM.fromList [(Text.pack section, [(varF (Text.pack variable), Text.pack value)])]) []

-- | If we have a 'Reader' and an 'Info' for 'a', then record the value in our
-- output 'Ini'.
recording1 :: Reader a -> Info a -> M a -> M a
recording1 rdr i = fmap runIdentity . recordingN rdr i . fmap Identity

-- | If we have a 'Reader' and an 'Info' for 'a', then record a collection of
-- values in our output 'Ini'.
recordingN :: Foldable f => Reader a -> Info a -> M (f a) -> M (f a)
recordingN (Reader _psr ppr _dom) i (M (Compose (Compose m))) = M (Compose (Compose (fmap f <$> m)))
  where
    f (Pair (Const inis) (Failure e))  = Pair (Const inis) (Failure e)
    f (Pair (Const inis) (Success xs)) = Pair (Const (inis ++ map g (toList xs))) (Success xs)
      where g = mkIni id (optIniVariable i) . ppr

-- | If we have a 'Reader' and an 'Info' for 'a', then record a 'Map' of values in
-- our output 'Ini'.
recordingKV :: Reader a -> Info (String, a) -> M (Map String a) -> M (Map String a)
recordingKV (Reader _psr ppr _dom) i (M (Compose (Compose m))) = M (Compose (Compose (fmap f <$> m)))
  where
    f (Pair (Const inis) (Failure e)) = Pair (Const inis) (Failure e)
    f (Pair (Const inis) (Success x)) = Pair (Const (inis ++ map g (Map.toList x))) (Success x)
      where
        g (k, v) = mkIni (<> "." <> Text.pack k) (optIniVariable i) (ppr v)

-- | Returns a command-line parser for `optparse-applicative` package, which if
-- it successfully parses will produce an IO action, which when run gives back
-- either the errors encountered along the way or a successful parse.
mkParser
  :: String                         -- ^ Environment variable prefix
  -> Ini                            -- ^ Parsed ini-format file
  -> [(String, String)]             -- ^ Process environment
  -> Option a                       -- ^ Options
  -> Opt.Parser (IO (Either [ParseError] (a, Ini)))
mkParser envVarPrefix ini env =
  unpackM
  . runAp (mkParserOption envVarPrefix ini packedEnv)
  . getOption
  where
    packedEnv = map (bimap Text.pack Text.pack) env
    unpackM :: M a -> Opt.Parser (IO (Either [ParseError] (a, Ini)))
    unpackM (M m) = fmap f <$> getCompose (getCompose m)
      where
        f (Pair (Const inis) (Success x)) = Right (x, combineInis inis)
        f (Pair (Const    _) (Failure e)) = Left e
    combineInis :: [Ini] -> Ini
    combineInis xs = Ini.Ini
      (foldr (\i hm -> HM.unionWith (++) (Ini.iniSections i) hm) HM.empty xs)
      (foldr (\i gs -> union (Ini.iniGlobals i) gs) [] xs)

-- | This converts interprets a single carrier value of 'F' as an 'M'.  'runAp'
-- is used to combine them over an entire 'Ap F' structure.
--
-- Note that it has to recurse with 'runAp' in the 'Commands' and 'WithIO'
-- cases.
mkParserOption :: String -> Ini -> [(Text, Text)] -> F a -> M a
mkParserOption envVarPrefix ini env = go
  where
    go :: F a -> M a
    go = \case
      One rdr@(Reader _psr ppr _dom) i ->
        case findValue envVarPrefix ini env rdr (fmap ppr i) of
          Failure es -> liftErrs es
          Success x  -> recording1 rdr i (liftPsr (one x i rdr <|> maybe empty pure (optValue i)))
      Optional rdr@(Reader _psr ppr _dom) i ->
        case findValue envVarPrefix ini env rdr (fmap ppr i) of
          Failure es -> liftErrs es
          Success xM -> recordingN rdr i (liftPsr ((Just <$> one xM i rdr) <|> pure Nothing))
      Many rdr@(Reader _psr ppr _dom) i ->
        case findValues envVarPrefix ini env rdr (fmap ppr i) of
          Failure es -> liftErrs es
          Success xs -> recordingN rdr i (liftPsr (more i rdr <|> pure xs))
      Some rdr@(Reader _psr ppr _dom) i ->
        case findValues envVarPrefix ini env rdr (fmap ppr i) of
          Failure es -> liftErrs es
          Success [] -> recordingN rdr i (liftPsr (ne <$>  some (one Nothing i rdr)))
          Success xs -> recordingN rdr i (liftPsr (ne <$> (some (one Nothing i rdr) <|> pure xs)))
        where ne = maybe (error "unreachable") (uncurry (NE.:|)) . uncons
      Map rdr@(Reader _psr ppr _dom) i ->
        case findValuesMap envVarPrefix ini env rdr (fmap (\(k,v) -> k ++ "=" ++ ppr v) i) of
          Failure es -> liftErrs es
          Success m  -> recordingKV rdr i (liftPsr (Map.fromList <$> kv i rdr <|> pure m))
      Commands i cmds ->
        case findValue envVarPrefix ini env (lookupReader cmds) i of
          Failure es                  -> liftErrs es
          Success Nothing             -> flags cmds i
          Success (Just (_, (_, m'))) -> cc (uu (flags cmds i) <|> uu (runAp go m'))
      WithIO nm f m' ->
        let g (inis, Failure es) = pure (inis, Failure es)
            g (inis, Success x)  = f x >>= \case
              Left e  -> pure (inis, Failure [CheckError nm e])
              Right y -> pure (inis, Success y)
        in cc ((>>= g) <$> uu (runAp go m'))
    -- Build a command line parser to read a single value.
    one :: Maybe a -> Info a -> Reader a -> Opt.Parser a
    one dflt i (Reader psr ppr _dom) =
      Opt.option (Opt.eitherReader psr) $ mconcat $
        (longO i <> shortO i <> helpO i <> metavarO i)
        : [Opt.value x <> Opt.showDefaultWith ppr | x <- maybeToList dflt]
    -- Build a command line parser that reads at least one value.
    more :: Info a -> Reader a -> Opt.Parser [a]
    more i (Reader psr _ppr _dom) = some $
      Opt.option (Opt.eitherReader psr) $
        longO i <> shortO i <> helpO i <> metavarO i
    -- Build a command line parser that reads any number of values, in a
    -- "<key>=<value>" format.
    kv :: Info (String, a) -> Reader a -> Opt.Parser [(String, a)]
    kv i (Reader psr _ppr _dom) = many $
      Opt.option (Opt.eitherReader f) $
        longO i <> shortO i <> helpO i <> metavarO i
      where f x = case break (== '=') x of
              (k, '=':v) -> (,) k <$> psr v
              (_,     _) -> Left $ printf "Bad parse: %s, expected 'key=value' form" (show x)
    -- Build a command line parser to read a command switch.  It can define the
    -- subsequent options that can be parsed.
    flags :: [(String, (Maybe String, Ap F a))] -> Info String -> M a
    flags cmds i = cc $ Opt.subparser $ mconcat
      [ Opt.command nm
          (Opt.info (uu (recording1 str i (pure cmdNm) *> runAp go m') <**> Opt.helper) mempty)
      | (cmdNm, (chosenNmMay, m')) <- cmds
      , let nm = fromMaybe (printf "%s.%s.%s" (optSection i) (optVariable i) cmdNm) chosenNmMay
      ]

-- | Attempt to parse a value from an 'Ini' file and the environment.
findValue
  :: String -> Ini -> [(Text, Text)] -> Reader a -> Info String
  -> Validation [ParseError] (Maybe a)
findValue envVarPrefix ini env rdr@(Reader psr _ppr _dom) i =
  case envValue <|> iniValue of
    Nothing -> pure Nothing
    Just t  -> either (\e -> Failure [mkErr rdr i e]) (pure . Just) (psr (Text.unpack t))
  where
    iniSection = Text.pack (optSection i)
    iniKey     = Text.pack (optVariable i)
    envKey     = optEnvVar i envVarPrefix
    iniValue   = findHead (Ini.lookupValue iniSection iniKey ini)
    envValue   = lookup (Text.pack envKey) env

-- | Attempt to parse any number of values from an 'Ini' file and the
-- environment.  Supports the _0, _1, and _NONE environment variables.
findValues
  :: String -> Ini -> [(Text, Text)] -> Reader a -> Info String
  -> Validation [ParseError] [a]
findValues envVarPrefix ini env rdr@(Reader psr _ppr _dom) i =
  case envValues <|> iniValues of
    Nothing -> pure []
    Just ts -> traverse (either (\e -> Failure [mkErr rdr i e]) pure . psr . Text.unpack) ts
  where
    iniSection  = Text.pack (optSection i)
    iniKey      = Text.pack (optVariable i)
    envKeys     = map (Text.pack . printf "%s_%d" (optEnvVar i envVarPrefix)) [(0::Int)..]
    envKeyEmpty = Text.pack $ printf "%s_NONE" (optEnvVar i envVarPrefix)
    iniValues   = findHead (Ini.lookupArray iniSection iniKey ini)
    envValues   = case (takeJusts $ map (`lookup` env) envKeys, lookup envKeyEmpty env) of
      ([], Nothing) -> Nothing
      (xs, Nothing) -> Just xs
      ([], Just  _) -> Just []
      ( _, Just  _) -> Nothing

-- | Attempt to parse any number of values from <variable>.<key> style variables
-- from an 'Ini' file and the environment.  Each environment variable has as its
-- suffix its key in the key-value.  An empty map can be defined by the _NONE
-- environment variable.
findValuesMap
  :: String -> Ini -> [(Text, Text)] -> Reader a -> Info String
  -> Validation [ParseError] (Map String a)
findValuesMap envVarPrefix ini env rdr@(Reader psr _ppr _dom) i =
  case envValues <|> iniValues of
    Nothing -> pure Map.empty
    Just ts -> traverse (either (\e -> Failure [mkErr rdr i e]) pure . psr . Text.unpack) ts
  where
    iniSection  = Text.pack (optSection i)
    iniKeys     = prefixedBy "." (Text.pack (optVariable i)) $ fold $ Ini.keys iniSection ini
    envKeys     = prefixedBy "_" (Text.pack (optEnvVar i envVarPrefix))   $ map fst env
    envKeyEmpty = Text.pack $ printf "%s_NONE" (optEnvVar i envVarPrefix)
    iniValues   = Just $ lookupWith iniKeys (\v -> findHead (Ini.lookupValue iniSection v ini))
    envValues   = case (lookupWith envKeys (`lookup` env), lookup envKeyEmpty env) of
      (m, Nothing) | Map.null m -> Nothing
                   | otherwise  -> Just m
      (m, Just  _) | Map.null m -> Just m
                   | otherwise  -> Nothing
    lookupWith :: [(Text, Text)] -> (Text -> Maybe Text) -> Map String Text
    lookupWith pairs f = maybe Map.empty fold . findHead $ for pairs $ \(v, k) ->
      Map.singleton (Text.unpack k) <$> f v

mkErr :: Reader a -> Info String -> String -> ParseError
mkErr (Reader _psr ppr dom) i msg =
  ParseError (optIniVariable i) msg (optSample i) (Domain $ map ppr <$> dom)

prefixedBy :: Text -> Text -> [Text] -> [(Text, Text)]
prefixedBy sep p = mapMaybe $ \k -> (,) k <$> Text.stripPrefix (p <> sep) k

findHead :: Foldable f => f a -> Maybe a
findHead = find (const True)

takeJusts :: [Maybe a] -> [a]
takeJusts []          = []
takeJusts (Nothing:_) = []
takeJusts (Just x:xs) = x:takeJusts xs

longO :: Opt.HasName x => Info o -> Opt.Mod x a
longO i = foldMap Opt.long (optLongs i)

shortO :: Opt.HasName x => Info o -> Opt.Mod x a
shortO i = foldMap Opt.short (optShorts i)

helpO :: Info o -> Opt.Mod x a
helpO i = foldMap Opt.help (optHelp i)

metavarO :: Opt.HasMetavar x => Info o -> Opt.Mod x a
metavarO i = let Metavar v = optMetavar i in Opt.metavar v
