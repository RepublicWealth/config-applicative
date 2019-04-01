{- |

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
>   [ ("left",  Nothing, Left  <$> option str      (name "FOO" "message"))
>   , ("right", Nothing, Right <$> option (enum f) (name "FOO" "value"))
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

The @[FOO].value@ option uses a custom 'enum' 'Reader' which is case sensitive,
and a function from @Bar@ to 'String'.  The 'enum' 'Reader' requires the @Bar@
type to have instances of the 'Enum' and 'Bounded' type classes.

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

module Config.Applicative.Tutorial () where
