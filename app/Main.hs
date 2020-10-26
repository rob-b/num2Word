{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Generic

import qualified Data.Text.IO    as TIO
import           Lib             (convert, mkTarget)
import           System.Exit

data NumberToWordOptions =
  NumberToWordOptions Int
  deriving (Generic, Show)

instance ParseRecord NumberToWordOptions

main :: IO ()
main = do
  n <- getRecord "Number to word"
  case mkTarget n of
    Left err     -> TIO.putStrLn err >> exitFailure
    Right target -> convert target
