# chicken-rbtree

Sorted dictionary data structures based on red-black trees.

## Usage

```scheme
(import rb-tree)
```

## Documentation

The `rb-tree` library is based on the SML/NJ library implementation
of red-black trees, which is in turn based on Chris Okasaki's
implementation of red-black trees.  The delete function is based on
the description in Cormen, Leiserson, and Rivest.

The present implementation code defines a persistent map class
that implements an ordered dictionary mapping of keys to values.

Looking up an arbitrary key or the min/max keys requires no more key
comparisons than the depth of the tree, which is `O(log n)` where `n`
is the total number of keys in the tree.  A range scan finds its first
association in `O(log n)` time.  The union operations take time linear
in the total size of the two maps.

### Procedures

The persistent map instance is created by procedure `rb-tree-map`:

```
rb-tree-map :: KEY-COMPARE-PROC -> persistent-map
```

where KEY-COMPARE-PROC is a user-supplied function that takes two keys
and returns a negative, positive, or zero number depending on how the
first key compares to the second.

All maps are persistent: the operations that change a map return a
new map and leave the original unmodified.

`persistent-map` is a yasos class that contains the following operations:

* `persistent-map? OBJ` : returns `#t` if `OBJ` is a persistent map.
* `empty? MAP` : returns `#t` if the map has no associations.
* `size MAP` : returns the number of associations in the map.
* `get MAP KEY` : returns the (key . value) pair of the association with the given `KEY`. If there is no such association, an error is signalled. `KEY` must be comparable to the keys in the map by the key-compare procedure the map was created with.
* `get/default MAP KEY DEFAULT` : as `get`, but returns `DEFAULT` if there is no association with `KEY`. If `DEFAULT` is a procedure, it is called with no arguments and its result is returned.
* `get-value MAP KEY` : returns the value of the association with the given `KEY`. If there is no such association, an error is signalled.
* `get-value/default MAP KEY DEFAULT` : as `get-value`, but returns `DEFAULT` (or the result of calling it, if it is a procedure) if there is no association with `KEY`.
* `get-min MAP` : returns the (key . value) pair of the association with the smallest key, or `#f` if the map is empty.
* `get-max MAP` : returns the (key . value) pair of the association with the largest key, or `#f` if the map is empty.
* `put MAP KEY VALUE` : returns a new map that contains the given association. If the key was already in the map, its association is replaced.
* `update MAP KEY VALUE MERGE-FN` : returns a new map that contains the given association. If the key was already in the map, `(MERGE-FN VALUE OLD-VALUE)` gives the new value.
* `generate MAP P F G SEED` : starting from `SEED`, adds the association `(F SEED)`, a (key . value) pair, to the map and continues with `(G SEED)`, until `(P SEED)` is true. Returns the new map.
* `delete MAP KEY` : returns a pair `(ASSOCIATION . NEW-MAP)`, where `ASSOCIATION` is the deleted (key . value) pair and `NEW-MAP` no longer contains it. If the key is not in the map, an error is signalled.
* `for-each-ascending MAP PROC` : applies `PROC` to each (key . value) association of the map, in ascending order of keys. Does nothing on an empty map.
* `for-each-descending MAP PROC` : applies `PROC` to each (key . value) association of the map, in descending order of keys. Does nothing on an empty map.
* `for-each-range MAP LO HI PROC` : applies `PROC` to each (key . value) association whose key lies in the half-open interval `[LO, HI)`, in ascending order of keys. `LO` or `HI` may be `#f`, which leaves that end of the interval open.
* `gen-range MAP LO HI` : returns a generator, a procedure of no arguments, that returns the (key . value) associations whose keys lie in `[LO, HI)` in ascending order of keys, and then an end-of-file object. `LO` and `HI` are as in `for-each-range`.
* `map-key-compare MAP` : returns the key-compare procedure the map was created with.

The map also implements these operations of `yasos-collections`:
`collection?`, `gen-keys` and `gen-elts` (generators of the keys and
of the values in ascending order of keys), and `for-each-key` and
`for-each-elt`.

### Unions

Each union takes two maps `M1` and `M2`, whose key-compare procedures
order keys the same way, and returns a new map whose keys are the
union of their keys. The new map uses the key-compare procedure of
`M1`. Neither input is modified.

* `((union-with MERGE-FN) M1 M2)` : `(MERGE-FN X Y)` gives the value of a key that is in both maps, where `X` is its value in `M1` and `Y` its value in `M2`. Keys in only one map keep their value.
* `((union-withi MERGE-FN) M1 M2)` : as `union-with`, but `MERGE-FN` also receives the key: `(MERGE-FN KEY X Y)`.
* `((union-transform FN MERGE-FN) M1 M2)` : the value of a key in only one map becomes `(FN X)`, and the value of a key in both maps becomes `(MERGE-FN X Y)`. `FN` and `MERGE-FN` are called in ascending order of keys. Returns a list `(N M KT1 KT2)`, where `N` is the number of keys in the new map `M`. `KT1` is a list of `(X . NEW-VALUE)` pairs that relates each value `X` of `M1` to its value in `M`, and `KT2` is the corresponding list for `M2`. A key in both maps contributes a pair to both lists. Both lists are in ascending order of keys.

### Checking a map

* `rb-tree-map-valid? MAP` : returns `#t` if the tree of `MAP` is ordered by its key-compare procedure, no red node has a red child, and every path from the root to a leaf has the same number of black nodes. It is intended for testing.

### Changes in this version

* `get-min` and `get-max` return `#f` on an empty map instead of signalling an error, and they, `for-each-ascending` and `for-each-descending` work on maps again.
* `delete` no longer loses other keys or unbalances the tree.
* `union-with`, `union-withi` and `union-transform` take two maps and return a map (or, for `union-transform`, a list that contains one).
* New operations: `for-each-range`, `gen-range`, `map-key-compare` and `rb-tree-map-valid?`.

## Examples

```scheme
(import scheme (chicken base) rb-tree (only yasos-collections size))

(define (compute-assoc key) (cons key (+ 1 key)))

(let* ((m (let recur ((m (rb-tree-map (lambda (x y) (- x y)))) (i -1))
            (let ((m1 (put m i (cdr (compute-assoc i)))))
              (if (< i 10) (recur m1 (+ 1 i)) m1)))))

  (print (get m 0))                      ; (0 . 1)
  (print (get/default m -2 'notfound))   ; notfound
  (print (get-min m) " " (get-max m))    ; (-1 . 0) (10 . 11)

  ;; traversing in ascending order
  (for-each-ascending m (lambda (association) (print association)))

  ;; the associations with keys in [3, 6)
  (for-each-range m 3 6 (lambda (association) (print association)))

  ;; a generator over the associations with keys from 8 upwards
  (let ((g (gen-range m 8 #f)))
    (let loop ((x (g)))
      (unless (eof-object? x)
        (print x)
        (loop (g)))))

  ;; deleting returns the removed association and the new map
  (let ((m1 (cdr (delete m 5))))
    (print (size m) " " (size m1)))      ; 12 11

  ;; union of two maps, adding the values of shared keys
  (let ((u ((union-with +) m (put (rb-tree-map (lambda (x y) (- x y))) 0 100))))
    (print (get-value u 0))))            ; 101
```

## License

>
> Copyright 2007-2018 Ivan Raikov
> 
>  This program is free software: you can redistribute it and/or modify
>  it under the terms of the GNU General Public License as published by
>  the Free Software Foundation, either version 3 of the License, or (at
>  your option) any later version.
>  
>  This program is distributed in the hope that it will be useful, but
>  WITHOUT ANY WARRANTY; without even the implied warranty of
>  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
>  General Public License for more details.
> 
>  A full copy of the GPL license can be found at
>  <http://www.gnu.org/licenses/>.

