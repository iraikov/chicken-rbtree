;;
;; Red-black tree
;;
;; An implementation of an ordered dictionary data structure, based
;; on red-black trees.
;;
;; This code is based on the SML/NJ library implementation of
;; red-black trees, which is in turn based on Chris Okasaki's
;; implementation of red-black trees.  The delete function is based on
;; the description in Cormen, Leiserson, and Rivest.
;;
;; Some helper code was borrowed from treap.scm by Oleg Kiselyov.
;;
;;
;; Copyright 2007-2026 Ivan Raikov.
;;
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;

(module rb-tree
	
  (
   rb-tree-map 
   union-with union-withi union-transform
   persistent-map? get get/default get-min get-max get-value get-value/default
   put generate update delete
   for-each-ascending for-each-descending
   for-each-range gen-range
   map-key-compare rb-tree-map-valid?)


  (import scheme (chicken base)
          (scheme base)
          (chicken memory representation)
          (only (chicken string) conc)
          datatype matchable yasos yasos-collections)
  (import-for-syntax srfi-1)

;;
;; Persistent map operations
;; 
;; A persistent map class provides the operations provided by yasos-collections,
;;  plus the following operations:
;;
;;	get 
;;
;;		procedure LAMBDA KEY . DEFAULT-CLAUSE which searches
;;		the given tree for an association with a given KEY, and
;;		returns a (key . value) pair of the found association.
;;		If an association with the KEY cannot be located in
;;		the tree, the PROC returns the result of evaluating the
;;		DEFAULT-CLAUSE.  If the default clause is omitted, an
;;		error is signalled.  The KEY must be comparable to the
;;		keys in the map by a key-compare predicate (which has
;;		been specified when the map was created)
;;
;;	get-min
;;
;;		returns a (key . value) pair for an association in the
;;		tree with the smallest key, or #f if the tree is empty.
;;
;;	get-max
;;
;;		returns a (key . value) pair for an association in the
;;		tree with the largest key, or #f if the tree is empty.
;;
;;
;;	for-each-ascending
;;
;;		a procedure LAMBDA PROC that will apply the given
;;		procedure PROC to each (key . value) association of
;;		the tree, from the one with the smallest key all the
;;		way to the one with the max key, in an ascending order
;;		of keys.
;;
;;	for-each-descending
;;
;;		a procedure LAMBDA PROC that will apply the given
;;		procedure PROC to each (key . value) association of
;;		the tree, in the descending order of keys.
;;
;;	for-each-range
;;
;;		a procedure LAMBDA LO HI PROC that applies PROC to each
;;		(key . value) association whose key lies in the
;;		half-open interval [LO, HI), in ascending order of
;;		keys.  LO or HI may be #f, meaning that end of the
;;		interval is open.  Finding the first association takes
;;		time proportional to the height of the tree.
;;
;;	gen-range
;;
;;		a procedure LAMBDA LO HI that returns a generator of
;;		the (key . value) associations whose keys lie in
;;		[LO, HI), in ascending order of keys.  The generator
;;		returns an end-of-file object when the range is
;;		exhausted.
;;
;;	map-key-compare
;;
;;		returns the key-compare procedure the map was
;;		created with.
;;

(define-predicate persistent-map?)
(define-operation (get pmap key))
(define-operation (get/default pmap key default-clause))
(define-operation (get-min pmap))
(define-operation (get-max pmap))
(define-operation (get-value pmap key))
(define-operation (get-value/default pmap key default-clause))
(define-operation (generate pmap p f g seed))
(define-operation (put pmap key value))
(define-operation (update pmap key value merge-fn))
(define-operation (delete pmap key))
(define-operation (for-each-ascending pmap f))
(define-operation (for-each-descending pmap f))
(define-operation (for-each-range pmap lo hi f))
(define-operation (gen-range pmap lo hi))
(define-operation (map-root pmap))
(define-operation (map-key-compare pmap))



;;
;; A red-black tree should satisfy the following two invariants:
;;
;;   Red Invariant: each red node has a black parent.
;;
;;   Black Condition: each path from the root to an empty node has the
;;     same number of black nodes (the tree's black height).
;;
;; The Red condition implies that the root is always black and the Black
;; condition implies that any node with only one child will be black and
;; its child will be a red leaf.
;;
;;
;; The red-black tree object is created by procedure 
;; rb-tree-map 
;;
;;  rb-tree-tree-persistent-map :: KEY-COMPARE-PROC -> PERSISTENT-MAP
;; 
;;  where KEY-COMPARE-PROC is a user-supplied function
;;
;;  KEY-COMPARE-PROC:: key1 key2 -> INTEGER
;; 
;;  that takes two keys and returns a negative, positive, or zero
;;  number depending on how the first key compares to the second.
;;


(define (rb-tree:error x . rest)
  (let ((port (open-output-string)))
    (let loop ((objs (cons x rest)))
      (if (null? objs)
	  (begin
	    (newline port)
	    (error 'rb-tree (get-output-string port)))
	  (begin (display (car objs) port)
		 (display " " port)
		 (loop (cdr objs)))))))


(define R 'Red)
(define B 'Black)

(define-record-type node
  (Tree color left key value right)
  node?
  (color node-color)
  (left node-left)
  (key node-key)
  (value node-value)
  (right node-right))

(define (Empty) #f)

(define (tree? x) (or (not x) (node? x)))

(define (red? t) (and t (eq? (node-color t) R)))
(define (black? t) (and t (eq? (node-color t) B)))

;; The zipper of a deletion: the path from the root to the current
;; subtree, innermost frame first.  A left frame records a node whose
;; left subtree is being worked on, and a right frame one whose right
;; subtree is.  The top of the path is #f.

(define-record-type left-frame
  (Left color key value tree zipper)
  left-frame?
  (color left-frame-color)
  (key left-frame-key)
  (value left-frame-value)
  (tree left-frame-tree)
  (zipper left-frame-zipper))

(define-record-type right-frame
  (Right color tree key value zipper)
  right-frame?
  (color right-frame-color)
  (tree right-frame-tree)
  (key right-frame-key)
  (value right-frame-value)
  (zipper right-frame-zipper))

(define (Top) #f)


(define (tree-tag x) (if x 'Tree 'Empty))


(set-record-printer! node
  (lambda (x out)
    (display "#(Tree " out)
    (display (conc (node-color x) " ") out)
    (display (tree-tag (node-left x)) out)
    (display (conc " " (node-key x) ":" (node-value x) " ") out)
    (display (tree-tag (node-right x)) out)
    (display ")" out)))


;;
;; This macro was borrowed from treap.scm by Oleg Kiselyov
;;
(define-syntax dispatch-on-key
  (er-macro-transformer
   (lambda (x r c)
     (let ((key-compare (second x)) 
           (key         (third x)) 	  
           (node-key    (fourth x))
           (on-less     (fifth x)) 
           (on-equal    (sixth x)) 
           (on-greater  (seventh x)))
       (let ((%let   (r 'let))
             (%cond  (r 'cond))
             (%else  (r 'else))
             (%zero?  (r 'zero?))
             (%positive?  (r 'positive?))
             (result (r 'result)))
         `(,%let ((,result (,key-compare ,key ,node-key )))
                 (,%cond
                  ((,%zero? ,result)     ,on-equal)
                  ((,%positive? ,result) ,on-greater)
                  (,%else                ,on-less))))))))


;; Adds a new association to the tree (or replaces the old one if existed).
(define (insert key-compare value-merge root key value)
  (let ins ((root root))
    (if (not root)
        (Tree R #f key value #f)
        (let ((color (node-color root))
              (a (node-left root))
              (yk (node-key root))
              (y (node-value root))
              (b (node-right root)))
          (dispatch-on-key
           key-compare key yk
           ;; Case 1: key < yk
           (if (red? a)
               (let ((c (node-left a)) (zk (node-key a)) (z (node-value a)) (d (node-right a)))
                 (dispatch-on-key
                  key-compare key zk
                  ;; Case 1.1: key < zk
                  (let ((c1 (ins c)))
                    (if (red? c1)
                        (Tree R (Tree B (node-left c1) (node-key c1) (node-value c1) (node-right c1))
                              zk z (Tree B d yk y b))
                        (Tree B (Tree R c1 zk z d) yk y b)))
                  ;; Case 1.2: key = zk
                  (Tree color (Tree R c key (value-merge value z) d) yk y b)
                  ;; Case 1.3: key > zk
                  (let ((d1 (ins d)))
                    (if (red? d1)
                        (Tree R (Tree B c zk z (node-left d1)) (node-key d1) (node-value d1)
                              (Tree B (node-right d1) yk y b))
                        (Tree B (Tree R c zk z d1) yk y b)))))
               (Tree B (ins a) yk y b))
           ;; Case 2: key = yk
           (Tree color a key (value-merge value y) b)
           ;; Case 3: key > yk
           (if (red? b)
               (let ((c (node-left b)) (zk (node-key b)) (z (node-value b)) (d (node-right b)))
                 (dispatch-on-key
                  key-compare key zk
                  ;; Case 3.1: key < zk
                  (let ((c1 (ins c)))
                    (if (red? c1)
                        (Tree R (Tree B a yk y (node-left c1)) (node-key c1) (node-value c1)
                              (Tree B (node-right c1) zk z d))
                        (Tree B a yk y (Tree R c1 zk z d))))
                  ;; Case 3.2: key = zk
                  (Tree color a yk y (Tree R c key (value-merge value z) d))
                  ;; Case 3.3: key > zk
                  (let ((d1 (ins d)))
                    (if (red? d1)
                        (Tree R (Tree B a yk y c) zk z
                              (Tree B (node-left d1) (node-key d1) (node-value d1) (node-right d1)))
                        (Tree B a yk y (Tree R c zk z d1))))))
               (Tree B a yk y (ins b))))))))


;; Looks for an item: Given a key, returns the corresponding (key
;; . value) association or #f if the tree does not contain an
;; association with that key.
(define (find-assoc key-compare root key)
  (let recur ((root root))
    (and root
         (dispatch-on-key key-compare key (node-key root)
                          (recur (node-left root))
                          (cons (node-key root) (node-value root))
                          (recur (node-right root))))))

;; Looks for an item: Given a key, returns the value of the
;; corresponding (key . value) association or #f if the tree does
;; not contain an association with that key.
(define (find-ref key-compare root key)
  (let recur ((root root))
    (and root
         (dispatch-on-key key-compare key (node-key root)
                          (recur (node-left root))
                          (node-value root)
                          (recur (node-right root))))))

;; Finds an association with a given key, and deletes it.  Returns
;; the (key . value) pair of the deleted association, or #f if it
;; couldn't be found

(define (delete-assoc key-compare root key)

  (define (zip zipper tree)
    (cond ((not zipper) tree)
          ((left-frame? zipper)
           (zip (left-frame-zipper zipper)
                (Tree (left-frame-color zipper) tree (left-frame-key zipper)
                      (left-frame-value zipper) (left-frame-tree zipper))))
          (else
           (zip (right-frame-zipper zipper)
                (Tree (right-frame-color zipper) (right-frame-tree zipper)
                      (right-frame-key zipper) (right-frame-value zipper) tree)))))

  ;; The black node with the fields of the black or red node T.
  (define (as-black t)
    (Tree B (node-left t) (node-key t) (node-value t) (node-right t)))
  (define (as-red t)
    (Tree R (node-left t) (node-key t) (node-value t) (node-right t)))

  ;; bbZip propagates a black deficit up the tree until either
  ;; the top is reached, or the deficit can be covered.  It
  ;; returns a boolean that is true if there is still a deficit
  ;; and the zipped tree.  The cases are tried in the order given.
  (define (bbZip zipper tree)
    (cond
     ((not zipper) (cons #t tree))
     ;; a red replacement node covers the deficit by turning black
     ((red? tree) (cons #f (zip zipper (as-black tree))))
     ((left-frame? zipper)
      (let ((color (left-frame-color zipper))
            (xk (left-frame-key zipper))
            (x (left-frame-value zipper))
            (s (left-frame-tree zipper))
            (z (left-frame-zipper zipper))
            (a tree))
        (cond
         ;; case 1L
         ((and (eq? color B) (red? s))
          (bbZip (Left R xk x (node-left s) (Left B (node-key s) (node-value s) (node-right s) z)) a))
         ;; case 3L
         ((and (black? s) (red? (node-left s)))
          (let ((l (node-left s)))
            (bbZip (Left color xk x
                         (Tree B (node-left l) (node-key l) (node-value l)
                               (Tree R (node-right l) (node-key s) (node-value s) (node-right s)))
                         z)
                   a)))
         ;; case 4L
         ((and (black? s) (red? (node-right s)))
          (cons #f (zip z (Tree color (Tree B a xk x (node-left s)) (node-key s) (node-value s)
                                (as-black (node-right s))))))
         ;; case 2L, red parent
         ((and (eq? color R) (black? s))
          (cons #f (zip z (Tree B a xk x (as-red s)))))
         ;; case 2L, black parent
         ((and (eq? color B) (black? s))
          (bbZip z (Tree B a xk x (as-red s))))
         (else (cons #f (zip zipper tree))))))
     (else
      (let ((color (right-frame-color zipper))
            (s (right-frame-tree zipper))
            (xk (right-frame-key zipper))
            (x (right-frame-value zipper))
            (z (right-frame-zipper zipper))
            (b tree))
        (cond
         ;; case 1R
         ((red? s)
          (bbZip (Right R (node-right s) xk x (Right B (node-left s) (node-key s) (node-value s) z)) b))
         ;; case 3R
         ((and (black? s) (red? (node-right s)))
          (let ((r (node-right s)))
            (bbZip (Right color
                          (Tree B (Tree R (node-left s) (node-key s) (node-value s) (node-left r))
                                (node-key r) (node-value r) (node-right r))
                          xk x z)
                   b)))
         ;; case 4R
         ((and (black? s) (red? (node-left s)))
          (cons #f (zip z (Tree color (as-black (node-left s)) (node-key s) (node-value s)
                                (Tree B (node-right s) xk x b)))))
         ;; case 2R, red parent
         ((and (eq? color R) (black? s))
          (cons #f (zip z (Tree B (as-red s) xk x b))))
         ;; case 2R, black parent
         ((and (eq? color B) (black? s))
          (bbZip z (Tree B (as-red s) xk x b)))
         (else (cons #f (zip zipper tree))))))))

  (define (delMin tree z)
    (cond ((not tree) (rb-tree:error 'delete "invalid tree"))
          ((not (node-left tree))
           (if (eq? (node-color tree) R)
               (values (node-key tree) (node-value tree) (cons #f (zip z (node-right tree))))
               (values (node-key tree) (node-value tree) (bbZip z (node-right tree)))))
          (else
           (delMin (node-left tree)
                   (Left (node-color tree) (node-key tree) (node-value tree) (node-right tree) z)))))

  (define (join color a b z)
    (cond ((and (eq? color R) (not a) (not b)) (zip z #f))
          ((not b) (cdr (bbZip z a)))
          ((not a) (cdr (bbZip z b)))
          (else
           (let-values (((xk x b) (delMin b (Top))))
             (if (car b)
                 ;; the deficit is in the new right subtree
                 (cdr (bbZip (Right color a xk x z) (cdr b)))
                 (zip z (Tree color a xk x (cdr b))))))))

  (define (del tree key z)
    (and tree
         (let ((color (node-color tree)) (a (node-left tree)) (yk (node-key tree))
               (y (node-value tree)) (b (node-right tree)))
           (dispatch-on-key key-compare
                            key yk
                            (del a key (Left color yk y b z))
                            (cons (cons yk y) (join color a b z))
                            (del b key (Right color a yk y z))))))

  (del root key (Top)))


;; The tree-level helpers below have names distinct from the
;; operations get-min, get-max, for-each-ascending and
;; for-each-descending, so that they do not shadow those operations.

(define (tree-min root)
  (and root
       (let recur ((t root))
         (if (node-left t)
             (recur (node-left t))
             (cons (node-key t) (node-value t))))))


(define (tree-max root)
  (and root
       (let recur ((t root))
         (if (node-right t)
             (recur (node-right t))
             (cons (node-key t) (node-value t))))))


(define (tree-for-each-ascending root f)
  (let appf ((t root))
    (when t
      (appf (node-left t))
      (f (cons (node-key t) (node-value t)))
      (appf (node-right t)))))


(define (tree-for-each-descending root f)
  (let appf ((t root))
    (when t
      (appf (node-right t))
      (f (cons (node-key t) (node-value t)))
      (appf (node-left t)))))


;; Range traversal over the half-open key interval [lo, hi).  Either
;; bound may be #f, meaning that end of the range is open.  The
;; traversal keeps a stack of nodes whose left subtrees have already
;; been visited; range-start builds the initial stack by descending
;; from the root towards lo, which takes time proportional to the
;; height of the tree.

(define (range-start key-compare root lo)
  (let descend ((t root) (stack '()))
    (cond ((not t) stack)
          ((and lo (negative? (key-compare (node-key t) lo)))
           (descend (node-right t) stack))
          (else (descend (node-left t) (cons t stack))))))


(define (range-push-left t stack)
  (if t
      (range-push-left (node-left t) (cons t stack))
      stack))


(define (in-range? key-compare hi k)
  (or (not hi) (negative? (key-compare k hi))))


(define (tree-for-each-range key-compare root lo hi f)
  (let loop ((stack (range-start key-compare root lo)))
    (when (pair? stack)
      (let ((t (car stack)))
        (when (in-range? key-compare hi (node-key t))
          (f (cons (node-key t) (node-value t)))
          (loop (range-push-left (node-right t) (cdr stack))))))))


(define (range-generator key-compare root lo hi)
  (let ((s (make-root (range-start key-compare root lo))))
    (lambda ()
      (let ((stack (root-val s)))
        (if (null? stack)
            (eof-object)
            (let ((t (car stack)))
              (if (in-range? key-compare hi (node-key t))
                  (begin
                    (root-val-set! s (range-push-left (node-right t) (cdr stack)))
                    (cons (node-key t) (node-value t)))
                  (begin
                    (root-val-set! s '())
                    (eof-object)))))))))
  
(define (apply-default-clause label key default-clause)
  (cond
   ((null? default-clause)
    (rb-tree:error label "key " key " was not found in the tree"))
   ((pair? (cdr default-clause))
    (rb-tree:error label "default argument must be a single clause"))
   ((procedure? (car default-clause)) ((car default-clause)))
   (else (car default-clause))))

;; A generator of (PROJECT node) for the nodes of the tree in
;; ascending order of keys.
(define (node-generator root project)
  (let ((s (make-root (range-push-left root '()))))
    (lambda ()
      (let ((stack (root-val s)))
        (if (null? stack)
            (eof-object)
            (let ((t (car stack)))
              (root-val-set! s (range-push-left (node-right t) (cdr stack)))
              (project t)))))))


;; Support for constructing red-black trees in linear time from
;; increasing ordered sequences (based on a description by R. Hinze).
;; The digits form a number in the bijective base-2 system: each digit
;; is One or Two, and Zero only marks the end of the list.  The trees
;; held by the digit at position i all have black height i.  Note that
;; the elements in the digits are ordered with the largest on the
;; left, whereas the elements of the trees are ordered with the
;; largest on the right.

(define-datatype digit digit?
  (Zero)
  (One (key identity)  (value identity) (tree tree?) (digit digit?))
  (Two (key1 identity) (value1 identity) (tree1 tree?)
       (key2 identity) (value2 identity) (tree2 tree?) (digit digit?)))


;; Adds an item whose key is guaranteed to be larger than any key in
;; the digit list l.  A carry out of a Two digit joins its two trees
;; and its smaller item into one black node of the next black height.
(define (add-item ak a l)
  (define (incr k1 a1 t1 d)
    (cases digit d
	   (Zero () (One k1 a1 t1 (Zero)))
	   (One (k2 a2 t2 r) (Two k1 a1 t1 k2 a2 t2 r))
	   (Two (k2 a2 t2 k3 a3 t3 r)
		(One k1 a1 t1 (incr k2 a2 (Tree B t3 k3 a3 t2) r)))))
  (incr ak a (Empty) l))


;; Links the digits into a tree.  The tree t accumulated so far holds
;; the items larger than those of digit d, and has the black height
;; of the trees in d.  A One digit becomes a black node; a Two digit
;; becomes a black node with a red left child.  Both raise the black
;; height by one, so the result satisfies both invariants.
(define (link-all d)
  (let link ((t (Empty)) (d d))
    (cases digit d
	   (Zero () t)
	   (One (k a t1 r)
		(link (Tree B t1 k a t) r))
	   (Two (k1 a1 t1 k2 a2 t2 r)
		(link (Tree B (Tree R t2 k2 a2 t1) k1 a1 t) r)))))


;; Returns the associations of the tree as a list of (key . value)
;; pairs in ascending order of keys.
(define (tree->list root)
  (let recur ((t root) (acc '()))
    (if t
        (recur (node-left t) (cons (cons (node-key t) (node-value t)) (recur (node-right t) acc)))
        acc)))


;; Folds over the union of two ascending association lists in
;; ascending order of keys.  Each key is passed to exactly one of
;; ON-LEFT (key value acc), for keys only in XS, ON-RIGHT (key value
;; acc), for keys only in YS, or ON-BOTH (key x y acc), for keys in
;; both lists.  The key passed to ON-BOTH is the one from XS.
(define (fold-union key-compare on-left on-right on-both xs ys acc)
  (let recur ((xs xs) (ys ys) (acc acc))
    (match (cons xs ys)
	   ((() . ()) acc)
	   ((((xk . x) . xs1) . ())
	    (recur xs1 ys (on-left xk x acc)))
	   ((() . ((yk . y) . ys1))
	    (recur xs ys1 (on-right yk y acc)))
	   ((((xk . x) . xs1) . ((yk . y) . ys1))
	    (let ((c (key-compare xk yk)))
	      (cond ((negative? c) (recur xs1 ys (on-left xk x acc)))
		    ((positive? c) (recur xs ys1 (on-right yk y acc)))
		    (else          (recur xs1 ys1 (on-both xk x y acc)))))))))


;; Merges the trees of two maps in linear time.  Every procedure
;; returns the value to store in the result for its key; the
;; associations are then linked into a new tree.  Returns the key
;; compare procedure of M1 and the new tree.
(define (union-trees m1 m2 on-left on-right on-both)
  (let ((key-compare (map-key-compare m1)))
    (values key-compare
	    (link-all
	     (fold-union key-compare
			 (lambda (k x d) (add-item k (on-left k x) d))
			 (lambda (k y d) (add-item k (on-right k y) d))
			 (lambda (k x y d) (add-item k (on-both k x y) d))
			 (tree->list (map-root m1))
			 (tree->list (map-root m2))
			 (Zero))))))


;; Unions of maps.  Each union takes two maps M1 and M2, created by
;; rb-tree-map with key-compare procedures that order keys the same
;; way, and returns a new map whose domain is the union of their
;; domains.  The new map uses the key-compare procedure of M1.  The
;; inputs are not modified.  A union takes time linear in the sum of
;; the sizes of the two maps.
;;
;;  ((union-with MERGE-FN) M1 M2)
;;
;;	MERGE-FN :: x y -> value defines the value of a key that is
;;	in both maps, where x is its value in M1 and y its value in M2.
;;
;;  ((union-withi MERGE-FN) M1 M2)
;;
;;	as union-with, but MERGE-FN :: key x y -> value also receives
;;	the key.
;;
;;  ((union-transform FN MERGE-FN) M1 M2)
;;
;;	maps the value of every key in only one map through FN :: x ->
;;	value, and the value of every key in both maps through
;;	MERGE-FN :: x y -> value.  FN and MERGE-FN are called in
;;	ascending order of keys.  Returns a list (N M KT1 KT2), where
;;	N is the number of keys in the new map M, KT1 is a list of (x
;;	. value) pairs that relates each value x in M1 to its new value
;;	in M, and KT2 is the corresponding list for M2.  A key in both
;;	maps contributes a pair to both KT1 and KT2.  Both lists are in
;;	ascending order of keys.

(define (union-with merge-fn)
  (lambda (m1 m2)
    (let-values (((key-compare root)
		  (union-trees m1 m2
			       (lambda (k x) x)
			       (lambda (k y) y)
			       (lambda (k x y) (merge-fn x y)))))
      (rb-tree-map key-compare new-root: root))))


(define (union-withi merge-fn)
  (lambda (m1 m2)
    (let-values (((key-compare root)
		  (union-trees m1 m2
			       (lambda (k x) x)
			       (lambda (k y) y)
			       merge-fn)))
      (rb-tree-map key-compare new-root: root))))


(define (union-transform fn merge-fn)
  (lambda (m1 m2)
    (let ((n 0) (kt1 '()) (kt2 '()))
      (let-values (((key-compare root)
		    (union-trees m1 m2
				 (lambda (k x)
				   (let ((x1 (fn x)))
				     (set! n (+ 1 n))
				     (set! kt1 (cons (cons x x1) kt1))
				     x1))
				 (lambda (k y)
				   (let ((y1 (fn y)))
				     (set! n (+ 1 n))
				     (set! kt2 (cons (cons y y1) kt2))
				     y1))
				 (lambda (k x y)
				   (let ((v (merge-fn x y)))
				     (set! n (+ 1 n))
				     (set! kt1 (cons (cons x v) kt1))
				     (set! kt2 (cons (cons y v) kt2))
				     v)))))
	(list n (rb-tree-map key-compare new-root: root)
	      (reverse kt1) (reverse kt2))))))


;; Checks that the tree is ordered by key-compare, that no red node
;; has a red child, and that every path from the root to an empty
;; node has the same number of black nodes.  The root itself may be
;; red.
(define (tree-valid? key-compare root)
  ;; lo and hi are #f or a one-element list holding a bound
  (define (above? k lo) (or (not lo) (positive? (key-compare k (car lo)))))
  (define (below? k hi) (or (not hi) (negative? (key-compare k (car hi)))))
  ;; returns the black height of t, or #f if t is not valid
  (define (check t lo hi parent-red?)
    (cond ((not t) 0)
          ((not (node? t)) #f)
          (else
           (let ((red? (eq? (node-color t) R)) (k (node-key t)))
             (and (memq (node-color t) (list R B))
                  (above? k lo) (below? k hi)
                  (not (and red? parent-red?))
                  (let ((ha (check (node-left t) lo (list k) red?))
                        (hb (check (node-right t) (list k) hi red?)))
                    (and ha hb (= ha hb)
                         (if red? ha (+ 1 ha)))))))))
  (and (check root #f #f #f) #t))


(define (get-depth root)
  (let loop ((node root) (level 0))
    (if node
        (max (loop (node-left node) (+ 1 level))
             (loop (node-right node) (+ 1 level)))
        level)))


(define (get-size root)
  (let loop ((node root))
    (if node
        (+ 1 (loop (node-left node)) (loop (node-right node)))
        0)))

(define-record-type <root>
  (make-root val)
  root?
  (val root-val root-val-set!))


(define (value-generator root) (node-generator root node-value))


(define (key-generator root) (node-generator root node-key))




(define (rb-tree-map key-compare #!key
                     (new-root (Empty)))
  (let ( (root new-root) )
    (object
      ;; persistent map behaviors
      ((persistent-map? self) #t)
      ((size self) (get-size root))
      
      ((empty? self) (not root))
      
       ((get self key)
        (or (find-assoc key-compare root key)
            (apply-default-clause 'get key '())))
	  
       ((get/default self key default-clause)
        (or (find-assoc key-compare root key)
            (apply-default-clause 'get key (list default-clause))))
	  
       ((get-value self key)
         (or (find-ref key-compare root key)
             (apply-default-clause 'get-value key '())))
       
       ((get-value/default self key default-clause)
         (or (find-ref key-compare root key)
             (apply-default-clause 'get-value key (list default-clause))))
       
       ((get-min self)
        (tree-min root))
       
       ((get-max self)
        (tree-max root))
	  
       ((generate self p f g seed)
        (let recur ((new-root root) (seed seed))
          (if (p seed)
              (rb-tree-map key-compare new-root: new-root)
              (let* ((item (f seed))
                     (new-root (insert key-compare (lambda (x ax) x) new-root (car item) (cdr item))))
                (recur new-root (g seed))))))
	  
       ((put self key value)
        (let ((new-root (insert key-compare (lambda (x ax) x) root key value)))
          (rb-tree-map key-compare new-root: new-root)))
	  
       ((update self key value merge-fn)
        (let ((new-root (insert key-compare merge-fn root key value)))
          (rb-tree-map key-compare new-root: new-root)))
	  
       ((delete self key)
        (let ((item+tree (delete-assoc key-compare root key)))
          (if item+tree
              (cons (car item+tree)
                    (rb-tree-map key-compare new-root: (cdr item+tree)))
              (apply-default-clause 'delete key '()))))
       
       ((for-each-ascending self f)
        (tree-for-each-ascending root f))
       
       ((for-each-descending self f)
        (tree-for-each-descending root f))

       ((for-each-range self lo hi f)
        (tree-for-each-range key-compare root lo hi f))

       ((gen-range self lo hi)
        (range-generator key-compare root lo hi))

       ((map-root self) root)

       ((map-key-compare self) key-compare)

       ;; collection behaviors
       ((collection? self) #t)
       ((gen-keys self) (key-generator root))
       ((gen-elts self) (value-generator root))
       ((for-each-key self proc)
        (tree-for-each-ascending root (lambda (item) (proc (car item)))))
       ((for-each-elt self proc)
        (tree-for-each-ascending root (lambda (item) (proc (cdr item)))))
        
       ))
  )


;; Returns #t if the tree of map m satisfies the ordering and
;; red-black invariants, #f otherwise.
(define (rb-tree-map-valid? m)
  (tree-valid? (map-key-compare m) (map-root m)))


)
