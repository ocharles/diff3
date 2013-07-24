{-| An implementation of a 3-way merge algorithm. -}
module Data.Algorithm.Diff3 (Hunk(..), diff3, merge) where

import Data.Monoid (Monoid, mempty, mappend)

import qualified Data.Algorithm.Diff as Diff

--------------------------------------------------------------------------------
-- | A hunk is a collection of changes that occur in a document. A hunk can be
-- some changes only in A, only in B, in both A & B (equally), or conflicting
-- between A, B and the original document.
data Hunk a = ChangedInA [a] | ChangedInB [a] | Both [a] | Conflict [a] [a] [a]
  deriving (Eq, Show)


--------------------------------------------------------------------------------
-- | Perform a 3-way diff against 2 documents and the original document.
diff3 :: Eq a => [a] -> [a] -> [a] -> [Hunk a]
diff3 a o b = step (Diff.getDiff o a) (Diff.getDiff o b)
  where
    step [] [] = []
    step [] ob = toHunk [] ob
    step oa [] = toHunk oa []
    step oa ob =
      let (conflictHunk, ra, rb) = shortestConflict oa ob
          (matchHunk, ra', rb')  = shortestMatch ra rb
      in conflictHunk ++ matchHunk ++ step ra' rb'


--------------------------------------------------------------------------------
merge :: [Hunk a] -> Either [Hunk a] [a]
merge hunks = maybe (Left hunks) Right $ go hunks
  where
    go [] = Just []
    go ((Conflict _ _ _):_) = Nothing
    go ((ChangedInA as):t) = fmap (as ++) $ go t
    go ((ChangedInB bs):t) = fmap (bs ++) $ go t
    go ((Both xs):t) = fmap (xs ++) $ go t


--------------------------------------------------------------------------------
toHunk :: [Diff.Diff a] -> [Diff.Diff a] -> [Hunk a]
toHunk [] [] = mempty
toHunk a  [] = return $ ChangedInA $ map get a
toHunk [] b  = return $ ChangedInB $ map get b
toHunk a  b
  | all isB a && all isB b = return $ Both $ map get $ filter isA a
  | all isB a = return $ ChangedInB $ map get $ filter isA b
  | all isB b = return $ ChangedInA $ map get $ filter isA a
  | otherwise = return $ Conflict (map get $ filter isA a)
                                  (map get $ filter isO a)
                                  (map get $ filter isA b)

--------------------------------------------------------------------------------
get :: Diff.Diff t -> t
get (Diff.First x) = x
get (Diff.Second x) = x
get (Diff.Both x _) = x

--------------------------------------------------------------------------------
isA :: Diff.Diff t -> Bool
isA (Diff.First _) = False
isA _ = True
{-# INLINE isA #-}

--------------------------------------------------------------------------------
isO :: Diff.Diff t -> Bool
isO (Diff.Second _) = False
isO _ = True
{-# INLINE isO #-}


--------------------------------------------------------------------------------
isB :: Diff.Diff t -> Bool
isB (Diff.Both _ _) = True
isB _ = False
{-# INLINE isB #-}

--------------------------------------------------------------------------------
shortestMatch :: [Diff.Diff a] -> [Diff.Diff a] -> ([Hunk a], [Diff.Diff a], [Diff.Diff a])
shortestMatch oa ob = go oa ob [] []
  where
    go (x@(Diff.Both _ _):xs) (y@(Diff.Both _ _):ys) accX accY = go xs ys (accX ++ [x]) (accY ++ [y])
    go xs ys accX accY = (toHunk accX accY, xs, ys)


--------------------------------------------------------------------------------
shortestConflict :: [Diff.Diff a] -> [Diff.Diff a] -> ([Hunk a], [Diff.Diff a], [Diff.Diff a])
shortestConflict l r =
    let (hunk, rA, rB) = go l r
    in (uncurry toHunk hunk, rA, rB)
  where
    go [] b = (([], b), [], [])
    go a [] = ((a, []), [], [])
    go a b =
      let (as, ta) = break isB a
          (bs, tb) = break isB b
          am = sum $ map motion as
          bm = sum $ map motion bs
          (as', ta') = incurMotion bm ta
          (bs', tb') = incurMotion am tb
      in if am == bm
         then ((as, bs), ta, tb)
         else ((as ++ as', bs ++ bs'), [], []) <> go ta' tb'

    motion (Diff.Second _) = 0
    motion _ = 1


--------------------------------------------------------------------------------
incurMotion :: Int -> [Diff.Diff t] -> ([Diff.Diff t], [Diff.Diff t])
incurMotion _ [] = ([], [])
incurMotion 0 as  = ([], as)
incurMotion n (a@(Diff.Both _ _):as) = ([a], []) <> incurMotion (pred n) as
incurMotion n (a@(Diff.Second _):as) = ([a], []) <> incurMotion (pred n) as
incurMotion n (a:as) = ([a], []) <> incurMotion n as


--------------------------------------------------------------------------------
-- This is here so we can build on GHC 7.4.
infixr 6 <>

-- | An infix synonym for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
