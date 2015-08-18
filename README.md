# chicken-rbtree

Sorted dictionary data structures based on red-black trees.

## Usage

(require-extension rb-tree typeclass)

## Documentation

The `rb-tree` library is based on the SML/NJ library implementation
of red-black trees, which is in turn based on Chris Okasaki's
implementation of red-black trees.  The delete function is based on
the description in Cormen, Leiserson, and Rivest.

The present implementation code defines a persistent map typeclass
that implements an ordered dictionary mapping of keys to values.

Looking up an arbitrary or the min/max keys, and deleting the min/max
keys require no more key comparisons than the depth of the tree, which
is `O(log n)` where `n` is the total number of keys in the tree.

### Procedures

The persistent map instance is created by procedure `rb-tree-map`:

<procedure>rb-tree-map:: KEY-COMPARE-PROC [insdel-key-compare: KEY-COMPARE-PROC]  -> <PersistentMap></procedure>

where KEY-COMPARE-PROC is a user-supplied function that takes two keys
and returns a negative, positive, or zero number depending on how the
first key compares to the second.

Optional keyword argument `insdel-key-compare` can be used to
specify different key comparison predicates for the insertion and
deletion operations.

The `<PersistentMap>` typeclass contains the following operations:

* `empty` : returns a new empty tree
* `empty? TREE` : returns `#t` if the given tree is empty
* `get TREE` : returns a procedure of the form `(LAMBDA KEY . DEFAULT-CLAUSE` which searches the given tree for an association with a given `KEY`, and returns a (key . value) pair of the found association. If an association with `KEY` cannot be located in the  tree, the procedure returns the result of evaluating the `DEFAULT-CLAUSE`. If the default clause is omitted, an error is signalled. `KEY` must be comparable to the keys in the  tree by a key-compare predicate (which has been specified when the  tree was created)
* `get-value TREE` : returns a procedure of the form `(LAMBDA KEY . DEFAULT-CLAUSE` which searches the tree for an association with a given `KEY`, and returns the value of (key . value) pair of the found association. If an association with `KEY` cannot be located in the  tree, the procedure returns the result of evaluating the `DEFAULT-CLAUSE`. If the default clause is omitted, an error is signalled. `KEY` must be comparable to the keys in the  tree by a key-compare predicate (which has been specified when the  tree was created)
* `get-min TREE` : returns a (key . value) pair for an association in the tree with the smallest key. If the  tree is empty, an error is signalled.
* `get-max TREE` : returns a (key . value) pair for an association in the tree with the largest key. If the tree is empty, an error is signalled.
* `size TREE` : returns the size (the number of associations) in the tree
* `put TREE KEY VALUE` : returns a new tree object that contains the given association; if the key was already in the tree, the association will be replaced. 
* `update TREE KEY VALUE MERGE-FN` : returns a new  tree object that contains the given association; if the key was already in the tree, procedure `MERGE-FN` is used to merge the old and new values.
* `delete TREE KEY . DEFAULT-CLAUSE` : if the specified key is found, it returns a new tree object that no longer contains the association specified by that key, while the original tree object is unmodified. If the key is not found, the procedure returns the result of evaluating `DEFAULT-CLAUSE`
* `for-each-ascending TREE` : returns a procedure `LAMBDA PROC` that will apply the given procedure PROC to each (key . value) association of the tree, from the one with the smallest key all the way to the one with the max key, in an ascending order of keys. 
* `for-each-descending TREE` : returns a procedure `LAMBDA PROC` that will apply the given procedure `PROC`to each (key . value) association of the tree, in the descending order of keys. 
* `map TREE` : returns a procedure `LAMBDA PROC` that will apply the given procedure `PROC`to the value component of each association in the  tree, in the ascending order of keys, and will construct a copy of the tree that contains the values returned by that procedure.
* `mapi TREE` : returns a procedure `LAMBDA PROC` that will apply the given procedure `PROC`to each (key . value) association in the  tree, in the ascending order of keys, and will construct a copy of the tree that contains the values returned by that procedure.
* `fold TREE` : returns a procedure `LAMBDA PROC INITIAL` such that, given the associations in the tree ordered by the descending order of keys: `(key-n . value-n) ... (key-2 . value-2) (key-1 . value-1) ` the procedure returns the result of the successive function applications `(PROC value-1 (PROC value-2 ... (PROC value-n INITIAL)`. 
* `foldi TREE` : returns a procedure `LAMBDA PROC INITIAL` such that, given the associations in the tree ordered by the descending order of keys: `(key-n . value-n) ... (key-2 . value-2) (key-1 . value-1) ` the procedure returns the result of the successive function applications `(PROC key-1 value-1 (PROC key-2 value-2 ... (PROC key-n value-n INITIAL)`. 
* `fold-right TREE` : returns a procedure `LAMBDA PROC INITIAL` such that, given the associations in the tree ordered by the ascending order of keys: `(key-1 . value-1) (key-2 . value-2) ... (key-n . value-n) ` the procedure returns the result of the successive function applications `(PROC value-n ... (PROC value-2 (PROC value-1 INITIAL)`. 
* `foldi-right TREE` : returns a procedure `LAMBDA PROC INITIAL` such that, given the associations in the tree ordered by the ascending order of keys: `(key-1 . value-1) (key-2 . value-2) ... (key-n . value-n) ` the procedure returns the result of the successive function applications `(PROC key-n value-n ... (PROC key-2 value-2 (PROC key-1 value-1 INITIAL)`. 
* `fold-partial TREE` : returns a procedure `LAMBDA PRED PROC INITIAL` such that, given the associations in the tree ordered by the descending order of keys: `(key-n . value-n) ... (key-2 . value-2) (key-1 . value-1) ` the procedure returns the result of the successive function applications `(PROC value-i ... (PROC value-n INITIAL)`, where `i <= n` and `(PRED x)` holds true for all `x = (value-n) ... (value-i)`. In other words, this function acts like `fold` on the ordered subset of the values `x` in the tree such that `(PRED x)` is true. 
* `foldi-partial TREE` : returns a procedure `LAMBDA PRED PROC INITIAL` such that, given the associations in the tree ordered by the descending order of keys: `(key-n . value-n) ... (key-2 . value-2) (key-1 . value-1) ` the procedure returns the result of the successive function applications `(PROC key-i value-i ... (PROC key-n value-n INITIAL)`, where `i <= n` and `(PRED xk x)` holds true for all `x = (value-n) ... (value-i)` and `xk = (key-n) ... (key-i)`. In other words, this function acts like `foldi` on the ordered subset of the key-value pairs `(k . x)` in the tree such that `(PRED k x)` is true. 
* `fold-right-partial TREE` : returns a procedure `LAMBDA PRED PROC INITIAL` such that, given the associations in the tree ordered by the ascending order of keys: `(key-1 . value-1) (key-2 . value-2) ... (key-n . value-n) ` the procedure returns the result of the successive function applications `(PROC value-1 ... (PROC value-i INITIAL)`, where `i <= n` and `(PRED x)` holds true for all `x = (value-1) ... (value-i)`. In other words, this function acts like `fold-right` on the ordered subset of the values `x` in the tree such that `(PRED x)` is true. 
* `foldi-right-partial TREE` : returns a procedure `LAMBDA PRED PROC INITIAL` such that, given the associations in the tree ordered by the descending order of keys: `(key-1 . value-1) (key-2 . value-2) ... (key-1 . value-1) ` the procedure returns the result of the successive function applications `(PROC key-1 value-1 ... (PROC key-i value-i INITIAL)`, where `i <= n` and `(PRED xk x)` holds true for all `x = (value-1) ... (value-i)` and `xk = (key-1) ... (key-i)`. In other words, this function acts like `foldi-right` on the ordered subset of the key-value pairs `(k . x)` in the tree such that `(PRED k x)` is true. 
* `fold-limit TREE` : returns a procedure `LAMBDA PRED PROC INITIAL` such that, given the associations in the tree ordered by the descending order of keys: `(key-n . value-n) ... (key-2 . value-2) (key-1 . value-1) ` the procedure returns the result of the successive function applications `(PROC value-i ... (PROC value-n INITIAL)`, where `i <= n` and `(PRED x)` does not hold true for all `x = (PROC value-n INITIAL)  ... (PROC (value-i) (PROC value-(i-1)...`. 
* `fold-right-limit TREE` : returns a procedure `LAMBDA PRED PROC INITIAL` such that, given the associations in the tree ordered by the descending order of keys: `(key-1 . value-1) (key-2 . value-2) ... (key-i . value-1) ` the procedure returns the result of the successive function applications `(PROC value-i ... (PROC value-1 INITIAL)`, where `i <= n` and `(PRED x)` does not hold true for all `x = (PROC value-1 INITIAL)  ... (PROC (value-i) (PROC value-(i-1)...`. 


## Examples

```scheme

 (use rb-tree typeclass)
 
 (define (++ x) (fx+ 1 x))
 (define (-- x) (fx- x 1))
 
 (let ((m (rb-tree-map (lambda (x y) (- x y)))))
    (with-instance ((<PersistentMap> m))
      
      (let* ((compute-assoc (lambda (key) (cons key (++ key))))
             (min-key -1) (max-key 10)
             (t 
              (let recur  ((t (empty)) (i min-key))
                (let ((t1 (put t i (cdr (compute-assoc i)))))
                  (if (< i max-key) (recur t1 (++ i)) t1))))
             )
            
       (print ((get t) (++ min-key)))
 
       (print ((get t) (-- min-key) 'notfound))
 
       ;; checking traversing in ascending order
       (let ((expected-key min-key))
        ((for-each-ascending t)
         (lambda (association)
           (print (equal? association (compute-assoc expected-key)))
          (set! expected-key (++ expected-key)))))
  )
 ))
```

## License

>
> Copyright 2007-2015 Ivan Raikov
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

