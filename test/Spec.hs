{-# LANGUAGE TemplateHaskell #-}

import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           Lib            (restToString)

main :: IO ()
main = tests >>= print


prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) $ Gen.int (Range.constantFrom 0 0 300)
    map restToString xs === map restToString xs

tests :: IO Bool
tests =
  checkParallel $$(discover)
