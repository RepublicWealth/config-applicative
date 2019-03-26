{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

{- |

This module provides a unified method of configuring a program, being able to
read settings from a ini-format file, from environment variables, and from
the command line in a predictable and uniform manner, with room for
customisation.



= 'Option' Usage

As an example, let's create a simple data structure for a program's
configuration:

> data Foo = Foo
>   { fooInt     :: Int
>   , fooStrings :: [String]
>   , fooDoubles :: NonEmpty Double
>   , fooEnabled :: Map String Bool
>   , fooBar     :: Either String Bar
>   , fooFile    :: (FilePath, String)
>   } deriving Show
>
> data Bar = Frob | Wibble | Snarf
>   deriving (Bounded, Enum, Show)

We need an Option Int to parse in the Int:

> oInt :: Option Int
> oInt = option auto (name "FOO" "int")

This defines an 'Option' which when reading from an ini file will consult the
variable 'int' in the section @[FOO]@, when reading the enviroment variables it
will look for the @XXX_FOO_INT@ environment variable, and when parsing the
command line it will match the @--FOO.int@ option.  The XXX is a configurable
prefix for the environment variables.

This definition uses the 'auto' 'Reader', which uses a type's 'Show' and 'Read'
instances to parse the value from a string and to display it when required.

Now we need to read strings for @fooStrings@:

> oStrings :: Option [String]
> oStrings = optionMany str (name "FOO" "strings")

This 'Option' will read the variable @strings@ from the ini-file section @[FOO]@
as many times as it occurs, returning each one in a list.  When reading from the
environment variables it will search for @XXX_FOO_STRINGS_0@,
@XXX_FOO_STRINGS_1@, etc, or if @XXX_FOO_STRINGS_NONE@ exists the list will be
empty.  On the command line it will match @--FOO.strings@ options as many times
as they occur.  Here the 'str' 'Reader' is used, which simply returns the string
as read from the source.

> oDoubles :: Option (NonEmpty Double)
> oDoubles = optionSome auto (name "FOO" "doubles")

This option works in the same way as for 'optionMany', except that at least one
item needs to be read from some configuration source, and the @..._NONE@
environment variable is not used.  Again 'Show' and 'Read' instances for
'Double' are used in the 'auto' 'Reader'.

> oEnabled :: Option (Map String Bool)
> oEnabled = optionMap boolean (name "FOO" "enabled")

This 'Option' reads all variables of the form @enabled.\<key\>@ from section
@[FOO]@ in ini files, and collects the values with the keys in the 'Map'
returned.  In environment variables it searches for @XXX_FOO_ENABLED_\<KEY\>@,
and @XXX_FOO_ENABLED_NONE@ means an empty 'Map'.  On the command line
@--FOO.enabled@ is matched where the option arguments have the form
"\<key\>=\<value\>".  In this 'Option' the 'boolean' 'Reader' is used which
accepts the strings \"true\" and \"false\", ignoring case.

> oBar :: Option (Either String Bar)
> oBar = commands (name "FOO" "bar")
>   [ ("left",  Nothing, Left  <$> option str           (name "FOO" "message"))
>   , ("right", Nothing, Right <$> option (enum True f) (name "FOO" "value"))
>   ]
>   where f Frob   = "fr...ob"
>         f Wibble = "WIBBLEWIBBLE"
>         f Snarf  = "snarF!!!"

This 'Option' first checks a command setting, called @[FOO].bar@ in ini files,
and @XXX_FOO_BAR@ in environment variables.  At the command line it defines two
mutually exclusive commands, @FOO.bar.left@ and @FOO.bar.right@.  These settings
define what options are then subsequently loaded, if the \"left\" value is used
then @[FOO].message@ is consulted as a String, if the \"right\" value is used
then @[FOO].value@ is used.

The @[FOO].value@ option uses a custom 'enum' 'Reader' which takes a flag saying
whether it is case sensitive, and a function from @Bar@ to 'String'.  The 'enum'
'Reader' requires the @Bar@ type to have instances of the 'Enum' and 'Bounded'
type classes.

> oFile :: Option (FilePath, String)
> oFile = withIO "read-file"
>           (\path -> (\contents -> Right (path, contents)) <$> readFile path)
>           (option str (name "FOO" "file"))

This 'Option' first reads a string from @[FOO].file@, and then uses that string
as a file path to read the contents of the file and return both in a tuple.  If
there is some problem the @'FilePath' -> 'IO' ('Either' 'String' ('FilePath',
'String'))@ function can return a 'Left' value with an error message, which will
be an error tagged with the label \"read-file\".  If the function raises an
exception it will not be caught.

Now that we have all the pieces to parse the components of a configuration, we
can combine them with the standard 'Applicative' combinators:

> oFoo :: Option Foo
> oFoo = Foo <$> oInt <*> oStrings <*> oDoubles <*> oEnabled <*> oBar <*> oFile



= Running a Parser

Once an 'Option' is defined, we need to run it to configure our program.  First
build a parser:

@
parser
  :: 'Ini' -> [('String', 'String')]
  -> 'Opt.Parser' ('IO' ('Either' ['ParseError'] (Foo, 'Ini')))
parser ini env = 'mkParser' \"XXX\" ini env oFoo
@

This gives us an @optparse-applicative@ command line parser which will yield an
IO action if successful.  We need to provide a parsed 'Ini' value (from the
@ini@ package) and the process environment which can be obtained from
'System.Environment.getEnvironment'.

The parser can be run with

@
getFoo
  :: 'Ini' -> [('String', 'String')]
  -> 'IO' ('Either' ['ParseError'] (Foo, 'Ini'))
getFoo ini env = 'Control.Monad.join' $ 'Opt.execParser' $ 'Opt.info' (parser ini env \<**\> 'Opt.helper') 'Opt.fullDesc'
@

Tying the pieces together:

@
main :: 'IO' ()
main = do
  iniE <- 'Ini.readIniFile' \"test.ini\"
  case iniE of
    'Left' err -> 'print' err
    'Right' ini -> do
      env <- 'System.Environment.getEnvironment'
      r <- getFoo ini env
      case r of
        'Left' errs -> 'mapM_' 'print' errs
        'Right' (foo, ini') -> do
          'putStr' $ 'Text.unpack' $ 'Ini.printIniWith' ('Ini.WriteIniSettings' 'Ini.EqualsKeySeparator') ini'
          'putStrLn' \"\"
          'print' foo
@

Now can we run the parser and receive back either a list of 'ParseErrors', or
the parsed @Foo@ value along with a new 'Ini' value which contains all the
actual final values that made up the @Foo@ we received.  If that 'Ini' value
were written out to a file and used in a subsequent run, without any environment
variables or command line options, we would still receive the same @Foo@ value.

>>> ./example --FOO.int 0 --FOO.strings xyz --FOO.strings uvw --FOO.doubles 3.2 --FOO.file test.txt FOO.bar.right --FOO.value "snarF!!!" --FOO.enabled abc=FALSE
[FOO]
int=0
strings=xyz
strings=uvw
doubles=3.2
enabled.abc=false
bar=right
value=snarF!!!
file=test.txt
<BLANKLINE>
Foo {fooInt = 0, fooStrings = ["xyz", "uvw"], fooDoubles = 3.2 :| [], fooEnabled = fromList [("abc",False)], fooBar = Right Snarf, fooFile = ("test.txt","this is the contents of test.txt\n")}



= Command line help

As with all optparse-applicative parsers, the usage information can be quite
helpful:

>>> ./example --help
Usage: ./example --FOO.int ARG [--FOO.strings ARG] --FOO.doubles ARG
                 [--FOO.enabled ARG] COMMAND --FOO.file ARG
<BLANKLINE>
Available options:
  -h,--help                Show this help text
<BLANKLINE>
Available commands:
  FOO.bar.left
  FOO.bar.right

This output is a bit sparse at the moment, but per-option help can be provided,
metavars customised, command line option aliases defined, and so forth, which
can make the @--help@ output much friendlier.



= Generating example ini files

A sample ini file to begin filling out can be easily generated using
'genExample':

>>> putStrLn $ genExample "XXX" oFoo
[FOO]
<BLANKLINE>
# Valid values are "left", "right".
# When bar is left the following variables are required:
#   - FOO.message
# When bar is right the following variables are required:
#   - FOO.value
# Override using:
#   - the XXX_FOO_BAR environment variable
#   - the FOO.bar.left command
#   - the FOO.bar.right command
bar = [left|right]
<BLANKLINE>
# May be given any number of times, but must be at least once.
# Override using:
#   - the XXX_FOO_DOUBLES_0, _1, _2... environment variables
#   - the --FOO.doubles command line option
doubles = <ARG>
<BLANKLINE>
# Valid values are "false", "true".
# May be given any number of times, with the key following the variable
# separated by a full stop.
# Override using:
#   - the XXX_FOO_ENABLED_NONE or XXX_FOO_ENABLED_<key>... environment
#     variables
#   - the --FOO.enabled command line option
# The argument must be of the form "key=value" in command line options.
enabled.<key> = <ARG>
<BLANKLINE>
# Override using:
#   - the XXX_FOO_FILE environment variable
#   - the --FOO.file command line option
file = <ARG>
<BLANKLINE>
# Override using:
#   - the XXX_FOO_INT environment variable
#   - the --FOO.int command line option
int = <ARG>
<BLANKLINE>
# Override using:
#   - the XXX_FOO_MESSAGE environment variable
#   - the --FOO.message command line option
message = <ARG>
<BLANKLINE>
# May be given any number of times, or not at all.
# Override using:
#   - the XXX_FOO_STRINGS_NONE or XXX_FOO_STRINGS_0, _1, _2... environment
#     variables
#   - the --FOO.strings command line option
strings = <ARG>
<BLANKLINE>
# Valid values are "fr...ob", "WIBBLEWIBBLE", "snarF!!!".
# Override using:
#   - the XXX_FOO_VALUE environment variable
#   - the --FOO.value command line option
value = <ARG>

-}


module Config.Applicative
  (
    -- * Options types
    Option, option, optionMaybe, optionMany, optionSome, optionMap, commands
  , check, withIO
    -- * Option modifiers
  , Info, name, long, short, envVar, help, metavar, value, example
    -- * Reader and Pretty-Printing
  , Reader, str, text, auto, boolean, enum, enumCI, nonNegative, positive, password
  , maybeReader, eitherReader, lookupReader
    -- * Interpretations
  , Location(..), Example(..), Domain(..), ParseError(..)
  , mkParser, genExample
  ) where

import           Control.Applicative      (empty, many, some, (<**>), (<|>))
import           Control.Applicative.Free (Ap, liftAp, runAp, runAp_)
import           Control.Arrow            ((&&&))
import           Control.Monad            (when)
import           Data.Bifunctor           (bimap, second)
import           Data.Bool                (bool)
import           Data.Char                (isAlphaNum, toLower, toUpper)
import           Data.Foldable            (find, fold, toList)
import           Data.Function            (on)
import           Data.Functor.Compose     (Compose (Compose, getCompose))
import           Data.Functor.Const       (Const (Const))
import           Data.Functor.Identity    (Identity (Identity, runIdentity))
import           Data.Functor.Product     (Product (Pair))
import           Data.Ini                 (Ini)
import           Data.List                (groupBy, intercalate, sort, uncons,
                                           union)
import           Data.List.NonEmpty       (NonEmpty)
import           Data.Map.Strict          (Map)
import           Data.Maybe               (fromMaybe, mapMaybe, maybeToList)
import           Data.Set                 (Set)
import           Data.String              (IsString (fromString))
import           Data.Text                (Text)
import           Data.Traversable         (for)
import           Text.Printf              (printf)
import           Text.Read                (readMaybe)

import qualified Data.HashMap.Strict      as HM
import qualified Data.Ini                 as Ini
import qualified Data.List.NonEmpty       as NE
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import qualified Options.Applicative      as Opt

-- | Ini-file section and variable names
data Location = Location String String
  deriving (Eq, Ord, Show)

data ParseError
    -- | Error location (section+variable), message, possibly an example value
    -- that could have been used, and the domain of values if known.
  = ParseError Location String (Example String) Domain
    -- | A 'withIO' returned a 'Left' value.  First field is the 'withIO' label
    -- and the second is the message returned in the 'Left'.
  | CheckError String String
    deriving (Eq, Ord, Show)

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

-- | A 'Reader' actually represents both a parser and pretty-printer.
data Reader a
  = Reader (String -> Either String a) (a -> String) (Maybe [a])

-- | Simply pass through the parsed string unchanged.
str :: Reader String
str = Reader Right id Nothing

-- | Simply pass through the parsed string unchanged, but as a 'Text' value.
text :: Reader Text
text = Reader (Right . Text.pack) Text.unpack Nothing

-- | Use the 'Show' and 'Read' instances to parse and pretty-print.
auto :: (Read a, Show a) => Reader a
auto = Reader (\t -> maybe (Left ("Bad parse: " ++ show t)) Right $ readMaybe t) show Nothing

-- | Accepts @"true"@ or @"false"@ without caring about case.
boolean :: Reader Bool
boolean = enumCI (bool "false" "true")

-- | Generate a __case-sensitive__ parser from a pretty-printer if your type is
-- an instance of 'Enum' and 'Bounded'.
--
-- Using this with 'Data.Int.Int64' would be a bad idea because it contains so
-- many values, instead use it on small, hand-defined, enumeration types.
enum :: (Bounded a, Enum a) => (a -> String) -> Reader a
enum f = Reader g f (Just [minBound..])
  where
    g t   = maybe (Left ("Bad parse: " ++ show t)) Right $ Map.lookup t names
    names = Map.fromList [(f x, x) | x <- [minBound..]]

-- | Generate a __case-insensitive__ parser from a pretty-printer if your type
-- is an instance of 'Enum' and 'Bounded'.
--
-- Using this with 'Data.Int.Int64' would be a bad idea because it contains so
-- many values, instead use it on small, hand-defined, enumeration types.
enumCI :: (Bounded a, Enum a) => (a -> String) -> Reader a
enumCI f = Reader g f (Just [minBound..])
  where
    g t   = maybe (Left ("Bad parse: " ++ show t)) Right $ Map.lookup (norm t) names
    names = Map.fromList [(norm (f x), x) | x <- [minBound..]]
    norm  = map toLower

-- | Accept a 'Num' that is zero or greater.
nonNegative :: (Read a, Show a, Ord a, Num a) => Reader a
nonNegative = Reader (\t -> maybe (Left ("Bad parse: " ++ show t)) (checkPos t) $ readMaybe t) show Nothing
  where checkPos t x = if x < 0 then Left (t ++ ": must not be less than 0") else Right x

-- | Accept a 'Num' that is one or greater.
positive :: (Read a, Show a, Ord a, Num a) => Reader a
positive = Reader f show Nothing
  where
    f t = do
      x <- maybe (Left ("Bad parse: " ++ show t)) pure $ readMaybe t
      when (x <= 0) $
        Left (t ++ ": must be greater than 0")
      pure x

-- | Pass the string through unchanged as a parser, but pretty-print as a constant
-- string.
password :: IsString a => String -> Reader a
password mask' = Reader (Right . fromString) (const mask') Nothing

-- | Parse successfully with 'Just'.
maybeReader :: (String -> Maybe a) -> (a -> String) -> Reader a
maybeReader psr ppr = Reader (\t -> maybe (Left ("Bad parse: " ++ show t)) Right $ psr t) ppr Nothing

-- | Parse successfully with 'Right'.
eitherReader :: (String -> Either String a) -> (a -> String) -> Reader a
eitherReader psr ppr = Reader psr ppr Nothing

-- | Look up the string in an alist.
lookupReader :: [(String, a)] -> Reader (String, a)
lookupReader cmds = Reader f fst (Just cmds)
  where
    f x = case x `lookup` cmds of
      Just cmd -> Right (x, cmd)
      Nothing  -> Left ("Bad parse: " ++ show x)

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

-- | Derive the default long command line option from the section and variable
-- names.
autoLong :: Info a -> Info a
autoLong i = i{ optLongs = optLongs i ++ [s ++ "." ++ v]  }
  where
    s = optSection i
    v = optVariable i

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




--
-- PARSING CONFIGURATION
--

-- | An 'Option' is a chain of descriptions of options that we'd like to parse.
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
    f (Pair (Const inis) (Success xs)) = Pair (Const (inis ++ map mkIni (toList xs))) (Success xs)
      where
        mkIni x =
          let section  = Text.pack (optSection i)
              variable = Text.pack (optVariable i)
              txtValue = Text.pack (ppr x)
          in Ini.Ini (HM.fromList [(section, [(variable, txtValue)])]) []

-- | If we have a 'Reader' and an 'Info' for 'a', then record a 'Map' of values in
-- our output 'Ini'.
recordingKV :: Reader a -> Info (String, a) -> M (Map String a) -> M (Map String a)
recordingKV (Reader _psr ppr _dom) i (M (Compose (Compose m))) = M (Compose (Compose (fmap f <$> m)))
  where
    f (Pair (Const inis) (Failure e)) = Pair (Const inis) (Failure e)
    f (Pair (Const inis) (Success x)) = Pair (Const (inis ++ map mkIni (Map.toList x))) (Success x)
      where
        mkIni (k, v) =
          let section  = Text.pack (optSection i)
              variable = Text.pack (optVariable i ++ "." ++ k)
              txtValue = Text.pack (ppr v)
          in Ini.Ini (HM.fromList [(section, [(variable, txtValue)])]) []

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
  ParseError loc msg (Example $ optExample i) (Domain $ map ppr <$> dom)
  where
    loc = Location (optSection i) (optVariable i)

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
metavarO i = Opt.metavar (optMetavar i)


--
-- GENERATING EXAMPLE INI FILES
--

-- Example file generation proceeds by first extracting relevant information
-- from an 'Option' value in a list of 'ExampleItem's, and then producing the
-- strings of the example file from those summaries.

newtype Domain    = Domain (Maybe [String]) deriving (Eq, Ord, Show)
newtype Metavar   = Metavar String          deriving (Eq, Ord, Show)
newtype Example a = Example (Maybe a)       deriving (Eq, Ord, Show)

data ExampleValue
  = -- | An ordinary value, anything that's not a Map or a Command.  The first
    -- field is the domain of values if it is known, the second field is the
    -- defined metavar (defaults to \"ARG\"), and the third is the example value
    -- to display (if any).  An example value supersedes the metavar.
    MetavarValue    Domain Metavar (Example String)
    -- | A key-value Map value.
  | MapMetavarValue Domain Metavar (Example (String, String))
    -- | A Commands value, first item is the names of the commands, second is an
    -- example command value.
  | CommandsValue [String] (Example String)
    deriving (Eq, Ord, Show)

-- | How many times can this option be repeated?  Informs the documentation
-- generated.
data ExampleRepeats
  = RepeatNo | RepeatOptional | RepeatMany | RepeatSome | RepeatMap
    deriving (Eq, Ord, Show)

-- | Tracks what defaults have been overridden by the user.
data ExampleNames = ExampleNames
  { ovEnvVar   :: String
  , ovShorts   :: String    -- Truly a list of 'Char'.
  , ovLongs    :: [String]
  , ovCommands :: [String]
  } deriving (Eq, Ord, Show)

-- | A summary of an option.
data ExampleItem = ExampleItem
  { exSection  :: String
  , exVariable :: String
  , exValue    :: ExampleValue
  , exNames    :: ExampleNames
  , exHelp     :: Maybe String
  , exRepeats  :: ExampleRepeats
    -- | Commands have subtrees and what will be parsed in the subtree depends
    -- on which command is used.  This dependency information can be listed in
    -- the documentation.
  , exChildren :: [(String, [(String, String)])]
  } deriving (Eq, Ord, Show)

-- | Generate a heavily-commented sample ini configuration file based on an
-- 'Option' definition.
genExample
  :: String    -- ^ Prefix to add to environment variables searched for.
  -> Option a
  -> String
genExample envVarPrefix = formatExampleItems . extractExampleItems envVarPrefix

extractExampleItems :: String -> Option a -> [ExampleItem]
extractExampleItems envVarPrefix = runAp_ go . getOption
  where
    go :: F a -> [ExampleItem]
    go = \case
      One      (Reader _psr ppr dom) i -> [exItem i (mval  ppr dom i) [] RepeatNo       []]
      Optional (Reader _psr ppr dom) i -> [exItem i (mval  ppr dom i) [] RepeatOptional []]
      Many     (Reader _psr ppr dom) i -> [exItem i (mval  ppr dom i) [] RepeatMany     []]
      Some     (Reader _psr ppr dom) i -> [exItem i (mval  ppr dom i) [] RepeatSome     []]
      Map      (Reader _psr ppr dom) i -> [exItem i (mmval ppr dom i) [] RepeatMap      []]
      Commands i cmds ->
        exItem i (cval i cmds) cmds RepeatNo
          (map (second (runAp_ (fmap (optSection &&& optVariable) . findInfos) . snd)) cmds)
        : concatMap (extractExampleItems envVarPrefix . Option . snd . snd) cmds
      WithIO _nm _f m -> extractExampleItems envVarPrefix (Option m)
    exItem i v cmds = ExampleItem (optSection i) (optVariable i) v (names i cmds) (optHelp i)
    mval  ppr dom i = MetavarValue    (Domain $ fmap (map ppr) dom) (Metavar $ optMetavar i) (Example $ ppr <$> optExample i)
    mmval ppr dom i = MapMetavarValue (Domain $ fmap (map ppr) dom) (Metavar $ optMetavar i) (Example $ second ppr <$> optExample i)
    cval i cmds     = CommandsValue (map fst cmds) (Example $ optExample i)
    findInfos :: F a -> [Info ()]
    findInfos = \case
      One      _ i -> [() <$ i]
      Optional _ i -> [() <$ i]
      Many     _ i -> [() <$ i]
      Some     _ i -> [() <$ i]
      Map      _ i -> [() <$ i]
      Commands i _ -> [() <$ i]
      WithIO _ _ m -> runAp_ findInfos m
    names :: Info b -> [(String, (Maybe String, Ap F a))] -> ExampleNames
    names i cs = ExampleNames
      (optEnvVar i envVarPrefix)
      (Set.toList (optShorts i))
      (optLongs i)
      (map cmdName cs)
      where cmdName (nm, (chosenNm, _)) =
              fromMaybe (printf "%s.%s.%s" (optSection i) (optVariable i) nm) chosenNm

-- Turn 'ExampleItem's into an example ini file.
formatExampleItems :: [ExampleItem] -> String
formatExampleItems =
  unlines
  . intercalate ["", ""]
  . map (concatMap (commentParagraph 80) . fmtGroup)
  . groupBy ((==) `on` exSection)
  . sort
  where
    fmtGroup :: [ExampleItem] -> [String]
    fmtGroup [] = error "unreachable"
    fmtGroup xs@(x0:_) =
      printf "[%s]" (exSection x0)  -- Section header, eg. "[SOLVER]"
      : concat
          [ [ "" ]
            ++ concatMap fmtHelp (maybeToList (exHelp x))
            ++ fmtDomain (exValue x)
            ++ fmtChildren (exVariable x) (exChildren x)
            ++ fmtRepeats (exRepeats x)
            ++ fmtNames (exRepeats x) (exNames x)
            ++ [ printf "%s = %s" (fmtVariable (exVariable x) (exValue x)) (fmtValue (exValue x)) ]
          | x <- xs]
    fmtDomain :: ExampleValue -> [String]
    fmtDomain (MetavarValue    (Domain (Just dom)) _ _) =
      [printf "# Valid values are %s." (intercalate ", " (map show dom))]
    fmtDomain (MapMetavarValue (Domain (Just dom)) _ _) =
      [printf "# Valid values are %s." (intercalate ", " (map show dom))]
    fmtDomain (CommandsValue dom _) =
      [printf "# Valid values are %s." (intercalate ", " (map show dom))]
    fmtDomain _ = []
    -- Just dump the help comments as given.
    fmtHelp :: String -> [String]
    fmtHelp h = [ "# " ++ ln | ln <- lines h ]
    -- When this is a `commands` option, list the dependent variables.
    fmtChildren :: String -> [(String, [(String, String)])] -> [String]
    fmtChildren ty xs = concat
      [ printf "# When %s is %s %s" ty v (case cs of
          [] -> "no other variables are required." :: String
          _  -> "the following variables are required:")
        : [ printf "#   - %s.%s" section variable | (section, variable) <- cs ]
      | (v, cs) <- xs ]
    -- Can this item be repeated?
    fmtRepeats :: ExampleRepeats -> [String]
    fmtRepeats = \case
      RepeatNo       -> []
      RepeatOptional -> ["# May be given once or not at all."]
      RepeatMany     -> ["# May be given any number of times, or not at all."]
      RepeatSome     -> ["# May be given any number of times, but must be at least once."]
      RepeatMap      -> ["# May be given any number of times, with the key following the variable separated by a full stop."]
    -- Describe the environment variable and command line options that can be used.
    fmtNames :: ExampleRepeats -> ExampleNames -> [String]
    fmtNames rep (ExampleNames envvar shorts longs cmds) =
      [ printf "# Override using:" ]
      ++ [ printf "#   - the %s" (envRep rep :: String)                ]
      ++ [ printf "#   - the -%c command line option" f  | f <- shorts ]
      ++ [ printf "#   - the --%s command line option" f | f <- longs  ]
      ++ [ printf "#   - the %s command" c               | c <- cmds   ]
      ++ [ printf "# The argument must be of the form \"key=value\" in command line options."
         | RepeatMap == rep ]
      where
        envRep = \case
          RepeatNo       -> printf "%s environment variable" envvar
          RepeatOptional -> printf "%s_NONE or %s_0 environment variables" envvar envvar
          RepeatMany     -> printf "%s_NONE or %s_0, _1, _2... environment variables" envvar envvar
          RepeatSome     -> printf "%s_0, _1, _2... environment variables" envvar
          RepeatMap      -> printf "%s_NONE or %s_<key>... environment variables" envvar envvar
    fmtVariable :: String -> ExampleValue -> String
    fmtVariable varName = \case
      MapMetavarValue _ _ (Example       Nothing) -> varName ++ ".<key>"
      MapMetavarValue _ _ (Example (Just (k, _))) -> varName ++ "." ++ k
      _                                           -> varName
    -- Output the default definition for the variable.
    fmtValue :: ExampleValue -> String
    fmtValue = \case
      MetavarValue    _ (Metavar x) (Example Nothing)        -> printf "<%s>" x
      MapMetavarValue _ (Metavar x) (Example Nothing)        -> printf "<%s>" x
      CommandsValue              xs (Example Nothing)        -> printf "[%s]" (intercalate "|" xs)
      MetavarValue    _           _ (Example (Just d))       -> d
      MapMetavarValue _           _ (Example (Just (_k, v))) -> v
      CommandsValue               _ (Example (Just d))       -> d


--
-- LINE WRAPPING
--

-- This code is for wrapping comment lines, also taking into account indenting
-- wrapped lines where there is a bullet point or other indentation at the start
-- of the original line.  It's fiddly and I don't exactly remember how it works,
-- if something's badly wrong it should probably just be rewritten.

commentParagraph :: Int -> String -> [String]
commentParagraph lineLength = \case
  '#':' ':ln -> foldLine lineLength "#" (findIndent ln) (spaceBreaks ln)
  '#':ln     -> foldLine lineLength "#" (findIndent ln) (spaceBreaks ln)
  ln         -> [ln]
  where
    foldLine refill prefix indent = go (refill - length prefix - 1) prefix
      where
        go _    acc [] = [acc]
        go fuel acc (w:ws)
          | fuel - length w - 1 < 0 =
              acc : go (refill - length prefix - length indent)
                       (prefix ++ indent)
                       (w:ws)
          | otherwise = go (fuel - length w - 1) (acc ++ " " ++ w) ws

findIndent :: String -> String
findIndent xs = case span (== ' ') xs of
  ([],       _) -> ""
  (ss, '-':xs') -> replicate (length ss + 1 + length (takeWhile (== ' ') xs')) ' '
  (ss,       _) -> replicate (length ss) ' '

spaceBreaks :: String -> [String]
spaceBreaks = eatWord
  where
    loop []       = []
    loop (' ':xs) = eatWord xs
    loop xs       = eatWord xs
    eatWord [] = []
    eatWord xs =
      let (ss, xs' ) = span (== ' ') xs
          (ws, xs'') = span (/= ' ') xs'
      in (ss ++ ws) : loop xs''

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
