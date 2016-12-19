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

-- The value in `Just a` has to match the next list item
-- `Nothing` is a wildcard that can match zero or more list items
matching :: (Eq a) => [Maybe a] -> [a] -> Bool
matching [] [] = True
matching [] bs = False
matching (Just a:as) (b:bs) = a == b && matching as bs
matching (Just a:as) [] = False
matching (as@(Nothing:as')) (bs@(_:bs')) = matching as' bs || matching as bs'
matching (Nothing:as') (bs@[]) = matching as' bs

matchLeft :: [Int] -> [Int] -> [Int] -> Bool
matchLeft left original right = matching (matcher $ diff3 left original right) left
  where matcher [] = []
        matcher (LeftChange as:hs) = map Just as ++ matcher hs
        matcher (RightChange as:hs) = Nothing : matcher hs
        matcher (Unchanged as:hs) = map Just as ++ matcher hs
        matcher (Conflict as os bs:hs) = map Just as ++ matcher hs

matchRight :: [Int] -> [Int] -> [Int] -> Bool
matchRight left original right = matching (matcher $ diff3 left original right) right
  where matcher [] = []
        matcher (LeftChange as:hs) = Nothing : matcher hs
        matcher (RightChange as:hs) = map Just as ++ matcher hs
        matcher (Unchanged as:hs) = map Just as ++ matcher hs
        matcher (Conflict as os bs:hs) = map Just bs ++ matcher hs

matchOriginal :: [Int] -> [Int] -> [Int] -> Bool
matchOriginal left original right = matching (matcher $ diff3 left original right) original
  where matcher [] = []
        matcher (LeftChange as:hs) = Nothing : matcher hs
        matcher (RightChange as:hs) = Nothing : matcher hs
        matcher (Unchanged as:hs) = map Just as ++ matcher hs
        matcher (Conflict as os bs:hs) = map Just os ++ matcher hs

main :: IO ()
main =
  let testOpts = mempty { topt_maximum_generated_tests = Just 1000 }
      runner = mempty { ropt_test_options = Just testOpts }
  in defaultMainWithOpts
       [ testProperty "Can make changes in left document" leftChanges
       , testProperty "Can make changes in right document" rightChanges
       , testProperty "Left/right identical changes conflict" identicalChanges
       , testProperty "The 'identity' merge always succeeds" identityMerge
       , testProperty "Left side of diff is part of left input" matchLeft
       , testProperty "Right side of diff is part of right input" matchRight
       , testProperty "Original parts of diff are part of original input" matchOriginal
       ]
       runner
