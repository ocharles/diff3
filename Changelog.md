# Changelog

## 0.3.1

- Build with GHC 8.4

---

## 0.3

0.3 includes two semantic changes from @sth:

### Made conflicts shorter

Conflicts sometimes include more elements than necessary:

```
GHCi> diff3 "14567" "1234567" "1567"
[Unchanged "1",Conflict "456" "23456" "56",Unchanged "7"]
```

56 shouldn't be included in the conflict.

This patch fixes the issue by only including additional elements for the diff 
that is "behind", instead of both diffs. It advances only the minimal amount 
necessary to bring both diffs in sync when searching for the end of a conflict.

This way less items are included in the conflict:

```
GHCi> diff3 "14567" "1234567" "1567"
[Unchanged "1",Conflict "4" "234" "",Unchanged "567"]
```

### Made motion calculation consistent

The motion function in `shortestConflict` counts how many elements of the *first*
sequence are in a part of a diff. The function `incurMotion` then tries to 
advance by that many elements, but counts elements in the *second* sequence of 
the diff.

This leads to inconsistent/wrong diffs:

```
GHCi> diff3 "145" "12345" "1245"
[Unchanged "1",Conflict "" "23" "24",Unchanged "4",LeftChange "5"]
```

Here 4 is mentioned in the conflict as well as afterwards as unchanged. The 
training `LeftChange 5` is really unchanged.

With swapped arguments:

```
GHCi> diff3 "1245" "12345" "145"
[Unchanged "1",Conflict "24" "234" "",Unchanged "5",RightChange "5"]
```

Here 4 is (unnecessarily) incorporated in the conflict and 5 is mentioned twice.

This pull request fixes the counting in incurMotion, correcting the above examples:

```
GHCi> diff3 "1245" "12345" "145"
[Unchanged "1",Conflict "2" "23" "",Unchanged "45"]
GHCi> diff3 "145" "12345" "1245"
[Unchanged "1",Conflict "" "23" "2",Unchanged "45"]
```

-----

## 0.2.0.3

- Work around a bug with test-framework

-----

## 0.2.0

- Now uses Diff 0.2 or higher

-----

## 0.1.2

- Fixed some cases where input would cause `diff3` to never terminate.
- Corrected the implementation of `diff3` to handle trailing conflicts
  correctly.
- Added the `merge` function to take a list of `Hunk`s and try and merge
  them into a single document.

-----

## 0.1.0.1

- Added some INLINE pragmas

-----

## 0.1

- Initial release
