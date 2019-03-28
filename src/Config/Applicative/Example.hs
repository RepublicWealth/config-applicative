{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

--
-- GENERATING EXAMPLE INI FILES
--

 {- |
Example file generation proceeds by first extracting relevant information
from an 'Option' value in a list of 'ExampleItem's, and then producing the
strings of the example file from those summaries.
-}

module Config.Applicative.Example
  ( genExample
  ) where

import Config.Applicative.Types
  (Domain(..), Example(..), F(..), Info(..), Metavar(..), Option(..), Reader(..))

import Control.Applicative.Free (Ap, runAp_)
import Control.Arrow            ((&&&))
import Data.Bifunctor           (second)
import Data.Function            (on)
import Data.List                (groupBy, intercalate, sort)
import Data.Maybe               (fromMaybe, maybeToList)
import Text.Printf              (printf)

import qualified Data.Set as Set

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
