module Config.Applicative.Reader
  ( str, text, auto, boolean, enum, enumCI, nonNegative, positive, password
  , maybeReader, eitherReader, lookupReader
  ) where

import Config.Applicative.Types (Reader(..))

import Control.Monad (when)
import Data.Bool     (bool)
import Data.Char     (toLower)
import Data.String   (IsString(fromString))
import Data.Text     (Text)
import Text.Read     (readMaybe)

import qualified Data.Map  as Map
import qualified Data.Text as Text

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
