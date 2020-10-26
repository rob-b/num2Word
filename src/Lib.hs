{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib (mkTarget, convert) where

import           Data.Either        (rights)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List          (intercalate)
import           Data.Maybe         (catMaybes)
import           Data.Text          (Text)
import qualified Data.Text          as T

data Target = Target { unTarget :: Int }

mkTarget :: Int -> Either Text Target
mkTarget n
  | n < 1 = Left "Target must be greater than 0"
  | n > 100000 = Left "Target must be smaller than 100000"
  | otherwise = Right $ Target n

data InvalidNumber
  = InvalidNumber Text
  | NoWordDefined Text
  deriving (Show)

data NumberRepr = NumberRepr
  { numThousands :: Int
  , numHundreds  :: Int
  , numTens      :: Int
  } deriving (Show)

numberMap :: IntMap String
numberMap =
  IntMap.fromList
    [ (1, "One")
    , (2, "Two")
    , (3, "Three")
    , (4, "Four")
    , (5, "Five")
    , (6, "Six")
    , (7, "Seven")
    , (8, "Eight")
    , (9, "Nine")
    , (10, "Ten")
    , (11, "Eleven")
    , (12, "Twelve")
    , (13, "Thirteen")
    , (14, "Fourteen")
    , (15, "Fifteen")
    , (16, "Sixteen")
    , (17, "Seventeen")
    , (18, "Eighteen")
    , (19, "Nineteen")
    , (20, "Twenty")
    , (30, "Thirty")
    , (40, "Forty")
    , (50, "Fifty")
    , (60, "Sixty")
    , (70, "Seventy")
    , (80, "Eighty")
    , (90, "Ninety")
    ]

convert :: Target -> IO ()
convert target = do
  let numRepr = mkNumberRepr target
  let thous = thousandsToString $ numThousands numRepr
  let hunds = hundredsToString $ numHundreds numRepr
  let tens = tensToString $ numTens numRepr
  putStrLn . renderFinal $ rights [thous, hunds, tens]

renderFinal :: [String] -> String
renderFinal [] = "hmmm....."
renderFinal [x] = x
renderFinal xs = renderFinal' $ reverse xs
  where
    renderFinal' (x':xs') =
      let pre = intercalate ", " (reverse xs')
      in pre ++ " and " ++ x'

mkNumberRepr :: Target -> NumberRepr
mkNumberRepr i =
  let (numThousands, rem') = divMod (unTarget i) 1000
      (numHundreds, numTens) = divMod rem' 100
  in NumberRepr {..}

renderMillion :: Int -> Either InvalidNumber [Char]
renderMillion i = do
  let (millions, rest) = divMod i 1000
  ms <- (++ " million") <$> tensToString millions
  ts <-
    if rest > 0
      then thousandsToString rest
      else (Right "")
  pure $ ms ++ ", " ++ ts

renderHundredThousands :: Int -> Either InvalidNumber [Char]
renderHundredThousands n =
  let (hs, rest) = divMod n 100
  in case hundredsToString hs of
       Left err -> Left err
       Right name ->
         case fmap ("and " ++) (tensToString rest) of
           Left _      -> pure $ name ++ " thousand"
           Right thing -> pure $ name ++ " " ++ thing ++ " thousand"

thousandsToString :: Int -> Either InvalidNumber String
thousandsToString n
  | n < 0 = Left $ InvalidNumber "Number must be between 1-??"
  | n >= 1000 = renderMillion n
  | n >= 100 = renderHundredThousands n
  | n > 0 = addSuffix (tensToString n)
  | otherwise = Left $ InvalidNumber "Unsure how to proceed"
  where
    addSuffix x = fmap (++ " thousand") x

hundredsToString :: Int -> Either InvalidNumber String
hundredsToString n
  | n < 1 || n > 10 = Left $ InvalidNumber "Number must be between 1-10"
  | otherwise = do
    name <- note (noWordDefined n) (IntMap.lookup n numberMap)
    pure $ name ++ " hundred"

tensToString :: Int -> Either InvalidNumber String
tensToString n
  | n < 1 || n > 99 = Left $ InvalidNumber "Number must be between 1-99"
  | n < 20 = note (noWordDefined n) $ IntMap.lookup n numberMap
  | otherwise = Right $ tensToString' n
  where
    tensToString' :: Int -> String
    tensToString' n' =
      let (tens, units) = divMod n' 10
          tens' = tens * 10
          items = [IntMap.lookup tens' numberMap, IntMap.lookup units numberMap]
      in intercalate "-" (catMaybes items)

-- helpers
note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

noWordDefined :: Int -> InvalidNumber
noWordDefined x = NoWordDefined $ "No word defined for " <> T.pack (show x)
