{-| An implementation of a 3-way merge algorithm. -}
module Data.Algorithm.Diff3 (
    Hunk(..)
  , diff3
  , merge
  -- * Non-structural equality
  , diff3By
  , mergeLeft
  , mergeRight
  ) where

import Data.Algorithm.Diff
import Data.Monoid (Monoid, mempty, mappend)
import Prelude hiding ((<>))

--------------------------------------------------------------------------------
-- | A hunk is a collection of changes that occur in a document. A hunk can be
-- some changes only in A, only in B, in both A & B (equally), or conflicting
-- between A, B and the original document.  All hunks take 3 constructors, which
-- are, in order - the elements in the left document, the original document, and
-- the right document. This order matches the order of parameters to 'diff3'.
--
-- For 'Unchanged', contains the value from both A, the original, and B, in
-- case you are using a form of equality that doesn't check all data (see
-- 'Diff').
data Hunk a = LeftChange [a]
            | RightChange [a]
            | Unchanged [(a,a,a)]
            | Conflict [a] [a] [a]
  deriving (Eq, Show)

instance Functor Hunk where
  fmap f (LeftChange ls) = LeftChange (map f ls)
  fmap f (RightChange rs) = RightChange (map f rs)
  fmap f (Unchanged os) = Unchanged (map (\(a,o,b) -> (f a, f o, f b)) os)
  fmap f (Conflict ls os rs) = Conflict (map f ls) (map f os) (map f rs)

--------------------------------------------------------------------------------
-- | Perform a 3-way diff against 2 documents and the original document. This
-- returns a list of 'Hunk's, where each 'Hunk' contains the original document,
-- a change in the left or right side, or is in conflict. This can be considered
-- a \'low level\' interface to the 3-way diff algorithm - you may be more
-- interested in 'merge'.
diff3 :: Eq a => [a] -> [a] -> [a] -> [Hunk a]
diff3 = diff3By (==)

--------------------------------------------------------------------------------
-- | A form of 'diff3' with no 'Eq' constraint.  Instead, an equality
-- predicate is taken as the first argument.
diff3By :: (a -> a -> Bool) -> [a] -> [a] -> [a] -> [Hunk a]
diff3By f a o b = step (getDiffBy f o a) (getDiffBy f o b)
  where
    step [] [] = []
    step [] ob = toHunk [] ob
    step oa [] = toHunk oa []
    step oa ob =
      let (conflictHunk, ra, rb) = shortestConflict oa ob
          (matchHunk, ra', rb')  = shortestMatch ra rb
      in conflictHunk ++ matchHunk ++ step ra' rb'


--------------------------------------------------------------------------------
-- | Merge a list of 'Hunk's with no conflicts.  If any conflicts are found
-- (and unresolved), 'Left' is returned with the original list.
--
-- If there is a substring of identical elements in all three input lists,
-- prefers the "original" elements.  This only matters in the case where
-- equality is non-structural.  See 'mergeLeft' and 'mergeRight' for other
-- options.
merge :: [Hunk a] -> Either [Hunk a] [a]
merge = mergeOn Nothing

--------------------------------------------------------------------------------
-- | Version of 'merge' where, if there is a substring of identical
-- elements in the original three lists, prefers the left side option.
-- Only differs from 'merge' in the case where equality is non-structural.
mergeLeft :: [Hunk a] -> Either [Hunk a] [a]
mergeLeft = mergeOn (Just False)

--------------------------------------------------------------------------------
-- | Version of 'merge' where, if there is a substring of identical
-- elements in the original three lists, prefers the left side option.
-- Only differs from 'merge' in the case where equality is non-structural.
mergeRight :: [Hunk a] -> Either [Hunk a] [a]
mergeRight = mergeOn (Just True)

mergeOn :: Maybe Bool -> [Hunk a] -> Either [Hunk a] [a]
mergeOn p hunks = maybe (Left hunks) Right $ go hunks
  where
    go [] = Just []
    go ((Conflict _ _ _):_) = Nothing
    go ((LeftChange l):t) = fmap (l ++) $ go t
    go ((RightChange r):t) = fmap (r ++) $ go t
    go ((Unchanged o):t) = fmap (map f o ++) $ go t
    f (a,o,b) = case p of
                  Just False -> a
                  Nothing    -> o
                  Just True  -> b


--------------------------------------------------------------------------------
toHunk :: [Diff a] -> [Diff a] -> [Hunk a]
toHunk [] [] = mempty
toHunk a  [] = [LeftChange $ takeSecond a]
toHunk [] b  = [RightChange $ takeSecond b]
toHunk a  b  = case (traverse getBoth a, traverse getBoth b) of
    (Just a', Just b') -> [Unchanged (zipWith comb a' b')]
    (Just _ , Nothing) -> [RightChange $ takeSecond b]
    (Nothing, Just _ ) -> [LeftChange  $ takeSecond a]
    (Nothing, Nothing) -> [Conflict (takeSecond a) (takeFirst a) (takeSecond b)]
  where
    comb (ox, x) (_, y) = (x, ox, y)

takeSecond :: [Diff a] -> [a]
takeSecond []            = []
takeSecond (Second x:xs) = x:takeSecond xs
takeSecond (Both x _:xs) = x:takeSecond xs
takeSecond (_:xs)        = takeSecond xs

takeFirst :: [Diff a] -> [a]
takeFirst []            = []
takeFirst (First x :xs) = x:takeFirst xs
takeFirst (Both x _:xs) = x:takeFirst xs
takeFirst (_:xs)        = takeFirst xs

getBoth :: Diff a -> Maybe (a, a)
getBoth (Both o x) = Just (o, x)
getBoth _          = Nothing
{-# INLINE getBoth #-}

-- isB :: Diff a -> Bool
-- isB (Both _ _) = True
-- isB _ = False
-- {-# INLINE isB #-}

--------------------------------------------------------------------------------
shortestMatch :: [Diff a] -> [Diff a] -> ([Hunk a], [Diff a], [Diff a])
shortestMatch oa ob = go oa ob [] []
  where
    go (x@(Both _ _):xs) (y@(Both _ _):ys) accX accY = go xs ys (accX ++ [x]) (accY ++ [y])
    go xs ys accX accY = (toHunk accX accY, xs, ys)


--------------------------------------------------------------------------------
shortestConflict :: [Diff a] -> [Diff a] -> ([Hunk a], [Diff a], [Diff a])
shortestConflict l r =
    let (hunk, rA, rB) = go l r
    in (uncurry toHunk hunk, rA, rB)
  where
    go [] b = (([], b), [], [])
    go a [] = ((a, []), [], [])
    go a b =
      let (as, ta) = break isBoth a
          (bs, tb) = break isBoth b
          am = sum $ map motion as
          bm = sum $ map motion bs
          (as', ta') = if bm > am then incurMotion (bm-am) ta else ([], ta)
          (bs', tb') = if am > bm then incurMotion (am-bm) tb else ([], tb)
      in if am == bm
         then ((as, bs), ta, tb)
         else ((as ++ as', bs ++ bs'), [], []) <> go ta' tb'

    isBoth (Both _ _) = True
    isBoth _ = False

    motion (Second _) = 0
    motion _ = 1


--------------------------------------------------------------------------------
incurMotion :: Int -> [Diff a] -> ([Diff a], [Diff a])
incurMotion _ [] = ([], [])
incurMotion 0 as  = ([], as)
incurMotion n (a@(Both _ _):as) = ([a], []) <> incurMotion (pred n) as
incurMotion n (a@(First _):as) = ([a], []) <> incurMotion (pred n) as
incurMotion n (a:as) = ([a], []) <> incurMotion n as


--------------------------------------------------------------------------------
-- This is here so we can build on GHC 7.4.
infixr 6 <>

-- | An infix synonym for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
