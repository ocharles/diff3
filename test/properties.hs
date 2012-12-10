module Main where

import Data.Algorithm.Diff3
import Data.Monoid (mempty)
import Test.Framework
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2

leftChanges :: [Int] -> [Int] -> Bool
leftChanges left original = merge (diff3 left original original) == Right left

rightChanges :: [Int] -> [Int] -> Bool
rightChanges right original = merge (diff3 original original right) == Right right

identicalChanges :: [Int] -> [Int] -> Property
identicalChanges changed original = (changed /= original) ==>
    conflicts (merge (diff3 changed original changed))
  where conflicts (Left _) = True
        conflicts (Right _) = False

identityMerge :: [Int] -> Bool
identityMerge as = merge (diff3 as as as) == Right as

main :: IO ()
main =
  let testOpts = mempty { topt_maximum_generated_tests = Just 5000 }
      runner = mempty { ropt_test_options = Just testOpts }
  in defaultMainWithOpts
       [ testProperty "Can make changes in left document" leftChanges
       , testProperty "Can make changes in right document" rightChanges
       , testProperty "Left/right identical changes conflict" identicalChanges
       , testProperty "The 'identity' merge always succeeds" identityMerge
       ]
       runner
