{-# LANGUAGE FlexibleContexts #-}

module Config.Applicative.Samples
  ( -- * Sample Configs
    sample0, sample1, sample2
  ) where

import qualified Config.Applicative as Cfg

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict    (Map)

-- SAMPLE 0

data Sample0 = Sample0 deriving Show

sample0 :: Cfg.Option Sample0
sample0 = pure Sample0

-- SAMPLE 1

newtype Sample1 = Sample1 String deriving Show

sample1 :: Cfg.Option Sample1
sample1 = Sample1 <$> Cfg.option Cfg.str (Cfg.name "XXX" "abc")

-- SAMPLE 2

data Bar = Frob | Wibble | Snarf
  deriving (Bounded, Enum, Show)

data Sample2 = Sample2
  { s2Int     :: Int
  , s2Strings :: [String]
  , s2Doubles :: NonEmpty Double
  , s2Enabled :: Map String Bool
  , s2Bar     :: Either String Bar
  , s2File    :: (FilePath, String)
  } deriving Show

sample2 :: Cfg.Option Sample2
sample2 = Sample2
  <$> Cfg.option     Cfg.auto    (Cfg.name "FOO" "int")
  <*> Cfg.optionMany Cfg.str     (Cfg.name "FOO" "strings")
  <*> Cfg.optionSome Cfg.auto    (Cfg.name "FOO" "doubles")
  <*> Cfg.optionMap  Cfg.boolean (Cfg.name "FOO" "enabled")
  <*> Cfg.commands (Cfg.name "FOO" "bar")
        (let f Frob   = "fr...ob"
             f Wibble = "WIBBLEWIBBLE"
             f Snarf  = "snarF!!!"
         in [ ("left",  Nothing, Left  <$> Cfg.option Cfg.str        (Cfg.name "FOO" "message"))
            , ("right", Nothing, Right <$> Cfg.option (Cfg.enumCI f) (Cfg.name "FOO" "value"))
            ])
  <*> Cfg.withIO "read-file"
        (\path -> (\contents -> Right (path, contents)) <$> readFile path)
        (Cfg.option Cfg.str (Cfg.name "FOO" "file"))
