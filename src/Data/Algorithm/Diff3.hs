{-| An implementation of a 3-way merge algorithm. -}
module Data.Algorithm.Diff3 (Hunk(..), diff3, merge) where

import Data.Algorithm.Diff
import Data.Monoid (Monoid, mempty, mappend)

--------------------------------------------------------------------------------
-- | A hunk is a collection of changes that occur in a document. A hunk can be
-- some changes only in A, only in B, in both A & B (equally), or conflicting
-- between A, B and the original document.  All hunks take 3 constructors, which
-- are, in order - the elements in the left document, the original document, and
-- the right document. This order matches the order of parameters to 'diff3'.
data Hunk a = LeftChange [a]
            | RightChange [a]
            | Unchanged [a]
            | Conflict [a] [a] [a]
  deriving (Eq, Show)

instance Functor Hunk where
  fmap f (LeftChange ls) = LeftChange (map f ls)
  fmap f (RightChange rs) = RightChange (map f rs)
  fmap f (Unchanged os) = Unchanged (map f os)
  fmap f (Conflict ls os rs) = Conflict (map f ls) (map f os) (map f rs)

--------------------------------------------------------------------------------
-- | Perform a 3-way diff against 2 documents and the original document. This
-- returns a list of 'Hunk's, where each 'Hunk' contains the original document,
-- a change in the left or right side, or is in conflict. This can be considered
-- a \'low level\' interface to the 3-way diff algorithm - you may be more
-- interested in 'merge'.
diff3 :: Eq a => [a] -> [a] -> [a] -> [Hunk a]
diff3 a o b = step (getDiff o a) (getDiff o b)
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
    go ((LeftChange l):t) = fmap (l ++) $ go t
    go ((RightChange r):t) = fmap (r ++) $ go t
    go ((Unchanged o):t) = fmap (o ++) $ go t


--------------------------------------------------------------------------------
toHunk :: [Diff a] -> [Diff a] -> [Hunk a]
toHunk [] [] = mempty
toHunk a  [] = [LeftChange $ takeSecond a]
toHunk [] b  = [RightChange $ takeSecond b]
toHunk a  b
  | all isB a && all isB b = [Unchanged $ takeFirst a]
  | all isB a = [RightChange $ takeSecond b]
  | all isB b = [LeftChange $ takeSecond a]
  | otherwise = [Conflict (takeSecond a) (takeFirst a) (takeSecond b)]

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

isB :: Diff a -> Bool
isB (Both _ _) = True
isB _ = False
{-# INLINE isB #-}

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
          (as', ta') = incurMotion bm ta
          (bs', tb') = incurMotion am tb
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
