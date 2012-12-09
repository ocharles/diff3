{-| An implementation of a 3-way merge algorithm. -}
module Data.Algorithm.Diff3 (Hunk(..), diff3) where

import Data.Algorithm.Diff
import Data.Monoid (Monoid, mempty, mappend)

--------------------------------------------------------------------------------
-- | A hunk is a collection of changes that occur in a document. A hunk can be
-- some changes only in A, only in B, in both A & B (equally), or conflicting
-- between A, B and the original document.
data Hunk a = ChangedInA [a] | ChangedInB [a] | Both [a] | Conflict [a] [a] [a]
  deriving (Eq, Show)


--------------------------------------------------------------------------------
-- | Perform a 3-way diff against 2 documents and the original document.
diff3 :: (Eq a) => [a] -> [a] -> [a] -> [Hunk a]
diff3 a o b = step (getDiff o a) (getDiff o b)
  where
    step :: [(DI, a)] -> [(DI, a)] -> [Hunk a]
    step [] [] = []
    step oa ob =
      let (conflictHunk, ra, rb) = shortestConflict oa ob
          (matchHunk, ra', rb')  = shortestMatch ra rb
      in conflictHunk ++ matchHunk ++ step ra' rb'


--------------------------------------------------------------------------------
toHunk :: [(DI, a)] -> [(DI, a)] -> [Hunk a]
toHunk [] [] = mempty
toHunk a  [] = return $ ChangedInA $ map snd a
toHunk [] b  = return $ ChangedInB $ map snd b
toHunk a  b
  | all isB a && all isB b = return $ Both $ map snd $ filter isA a
  | all isB a = return $ ChangedInB $ map snd $ filter isA b
  | all isB b = return $ ChangedInA $ map snd $ filter isA a
  | otherwise = return $ Conflict (map snd $ filter isA a)
                                  (map snd $ filter isO a)
                                  (map snd $ filter isA b)


--------------------------------------------------------------------------------
isA :: (DI, t) -> Bool
isA (F,_) = False
isA (_,_) = True
{-# INLINE isA #-}

--------------------------------------------------------------------------------
isO :: (DI, t) -> Bool
isO (S,_) = False
isO (_,_) = True
{-# INLINE isO #-}


--------------------------------------------------------------------------------
isB :: (DI, t) -> Bool
isB (B,_) = True
isB (_,_) = False
{-# INLINE isB #-}

--------------------------------------------------------------------------------
shortestMatch :: [(DI,a)] -> [(DI,a)] -> ([Hunk a], [(DI, a)], [(DI, a)])
shortestMatch oa ob = go oa ob [] []
  where
    go (x@(B,_):xs) (y@(B,_):ys) accX accY = go xs ys (accX ++ [x]) (accY ++ [y])
    go xs ys accX accY = (toHunk accX accY, xs, ys)


--------------------------------------------------------------------------------
shortestConflict :: [(DI,a)] -> [(DI,a)] -> ([Hunk a], [(DI, a)], [(DI, a)])
shortestConflict l r =
    let (hunk, rA, rB) = go l r
    in (uncurry toHunk hunk, rA, rB)
  where
    go a b =
      let (as, ta) = break isBoth a
          (bs, tb) = break isBoth b
          am = sum $ map motion as
          bm = sum $ map motion bs
          (as', ta') = incurMotion bm ta
          (bs', tb') = incurMotion am tb
      in if am == bm
         then ((as, bs), ta, tb)
         else ((as ++ as', bs ++ bs'), [], []) <> go ta' tb'

    isBoth (B,_) = True
    isBoth (_,_) = False

    motion (S,_) = 0
    motion _ = 1


--------------------------------------------------------------------------------
incurMotion :: Int -> [(DI, t)] -> ([(DI,t)], [(DI,t)])
incurMotion _ [] = ([], [])
incurMotion 0 as  = ([], as)
incurMotion n (a@(B,_):as) = ([a], []) <> incurMotion (pred n) as
incurMotion n (a@(S,_):as) = ([a], []) <> incurMotion (pred n) as
incurMotion n (a:as) = ([a], []) <> incurMotion n as


--------------------------------------------------------------------------------
-- This is here so we can build on GHC 7.4.
infixr 6 <>

-- | An infix synonym for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
