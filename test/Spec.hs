{-# LANGUAGE TemplateHaskell #-}

import           Data.Either    (isLeft)
import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           Lib            (mkTarget)

main :: IO ()
main = tests >>= print

withinBounds :: (Ord a, Num a) => a -> Bool
withinBounds n
  | n > 0 && n < 100001 = True
  | otherwise = False

prop_mkTarget :: Property
prop_mkTarget =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) $ Gen.int (Range.constantFrom 0 0 150000)
    let fn x = (x, mkTarget x)
    let results = map fn xs
    let invalidNumbers = [fst result | result <- results, isLeft (snd result)]

    -- there should be no errors where the value was within our expected bounds
    if null invalidNumbers
      then success
      else (filter withinBounds invalidNumbers) === []


tests :: IO Bool
tests =
  checkParallel $$(discover)
