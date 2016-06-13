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
;; Copyright 2007-2016 Ivan Raikov.
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
   union-with union-withi 
   <PersistentMap> 
   )

  (import scheme chicken)
  
  (require-extension datatype matchable typeclass)

  (import (only data-structures conc identity))


;;
;; Persistent map typeclass
;; 
;; A persistent map class provides the following operations:
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
;;		tree with the smallest key. If the tree is empty,
;;		an error is signalled.
;;
;;	get-max
;;
;;		returns a (key . value) pair for an association in the
;;		tree with the largest key. If the tree is empty, an
;;		error is signalled.
;;
;;	empty
;;		returns an empty tree
;;
;;	empty?
;;		returns #t if the tree is empty
;;
;;	size
;;
;;		returns the size (the number of associations) in the
;;		tree
;;
;;
;;	for-each-ascending
;;
;;		a procedure LAMBDA PROC that will apply the given
;;		procedure PROC to each (key . value) association of
;;		the tree, from the one with the smallest key all the
;;		way to the one with the max key, in an ascending order
;;		of keys.  The tree must not be empty.
;;
;;	for-each-descending
;;
;;		a procedure LAMBDA PROC that will apply the given
;;		procedure PROC to each (key . value) association of
;;		the tree, in the descending order of keys.
;;		The tree must not be empty.
;;

(define-class <PersistentMap> 
  empty
  empty?
  size
  get
  get-value
  get-min
  get-max
  list-keys
  list-values
  put
  update
  delete
  for-each-ascending
  for-each-descending
  map mapi
  fold foldi
  fold-right foldi-right
  fold-partial foldi-partial
  fold-right-partial foldi-right-partial
  fold-limit
  fold-right-limit
  )


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
;; rb-tree-ephemeral-map / rb-tree-persistent-map 
;;
;;  rb-tree-tree-persistent-map :: KEY-COMPARE-PROC -> PERSISTENT-MAP
;;  rb-tree-tree-ephemeral-map :: KEY-COMPARE-PROC -> EPHEMERAL-MAP
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

(define (color? x) (or (eq? x 'Red) (eq? x 'Black)))


(define-datatype tree tree?
  (Empty)
  (Tree  (color color?) (left tree?) (key identity) (value identity) (right tree?)))


(define-datatype zipper zipper?
  (Top) 
  (Left (color color?) (key identity) (value identity) (tree tree?) (zipper zipper?))
  (Right (color color?) (tree tree?) (key identity) (value identity) (zipper zipper?)))



(define (tree-tag x)
  (cases tree x
	 (Empty () 'Empty)
	 (Tree (c l k v r) 'Tree)))


(define-record-printer (tree x out)
  (cases tree x 
	 (Empty () (display "#(Empty)" out))
	 (Tree (c l k v r)
	       (display "#(Tree " out)
	       (display (conc c " ") out) 
	       (display (tree-tag l) out)
	       (display (conc " " k ":" v " ") out)
	       (display (tree-tag r) out)
	       (display ")" out))))


;;
;; This macro was borrowed from treap.scm by Oleg Kiselyov
;;
(define-syntax dispatch-on-key 
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
		 (,%else                ,on-less)))))))


;; Adds a new association to the tree (or replaces the old one if existed).
(define (insert key-compare value-merge root key value)
    (let ins ((root root))
      (cases tree root
	   (Empty ()  (Tree R (Empty) key value (Empty)))
	   (Tree (color a yk y b)  
		 (dispatch-on-key key-compare 
		      key yk 
		      ;; Case 1: key < yk
		      (match a
			     (($ tree 'Tree 'Red c zk z d)
			      (dispatch-on-key key-compare
			       key zk
			       ;; Case 1.1: key < zk
			       (let ((c1 (ins c)))
                                 (match c1
                                        (($ tree 'Tree 'Red e wk w f)
                                         (Tree R (Tree B e wk w f) zk z (Tree B d yk y b)))
                                        (else  (Tree B (Tree R c1 zk z d) yk y b))))
			       ;; Case 1.2: key = zk
			       (Tree color (Tree R c key (value-merge value z) d) yk y b)
			       ;; Case 1.3: key > zk
			       (let ((d1 (ins d)))
                                 (match d1
                                        (($ tree 'Tree 'Red e wk w f)
                                         (Tree R (Tree B c zk z e)  wk  w  (Tree B f yk y b)))
                                        (else (Tree B (Tree R c zk z d1) yk y b))))))
			     (else (let ((a1  (ins a)))
                                     (Tree B a1 yk y b))))
		      ;; Case 2: key  = yk
		      (Tree color a key (value-merge value y) b)
		      ;; Case 3: key  > yk
		      (match b
			     (($ tree 'Tree 'Red c zk z d)
			      (dispatch-on-key key-compare
			       key zk
			       ;; Case 3.1: key < zk
			       (let ((c1 (ins c)))
                                 (match c1
                                        (($ tree 'Tree 'Red e wk w f)
                                         (Tree R (Tree B a yk y e)  wk  w (Tree B f zk z d)))
                                        (else (Tree B a yk y (Tree R c1 zk z d)))))
			       ;; Case 3.2: key = zk
			       (Tree color a yk y (Tree R c key (value-merge value z) d))
			       ;; Case 3.3: key > zk
			       (let ((d1 (ins d)))
                                 (match d1
                                        (($ tree 'Tree 'Red e wk w f)
                                         (Tree R (Tree B a yk y c)  zk z (Tree B e wk w f)))
                                        (else (Tree B a yk y (Tree R c zk z d1)))))))
			     (else (let ((b1 (ins b)))
				     (Tree B a yk y b1))))))))
    )


;; Looks for an item: Given a key, returns the corresponding (key
;; . value) association or #f if the tree does not contain an
;; association with that key.
(define (find-assoc key-compare root)
  (lambda (key)
    (let recur ((root root))
      (cases tree root 
	     (Empty ()  #f)
	     (Tree (c a yk y b)
		   (dispatch-on-key key-compare 
				    key yk (recur a) (cons yk y) (recur b)))))
    ))

;; Looks for an item: Given a key, returns the value of the
;; corresponding (key . value) association or #f if the tree does
;; not contain an association with that key.
(define (find-ref key-compare root)
  (lambda (key)
    (let recur ((root root))
      (cases tree root 
	     (Empty ()  #f)
	     (Tree (c a yk y b)
		   (dispatch-on-key key-compare 
				    key yk (recur a) y (recur b)))))
    ))

;; Finds an association with a given key, and deletes it.  Returns
;; the (key . value) pair of the deleted association, or #f if it
;; couldn't be found

(define (delete-assoc key-compare root key)

  (define (zip zipper tree)
    (match (cons zipper tree)
	   ((($ zipper 'Top) . a)  tree)
	   ((($ zipper 'Left color xk x b z) . a)   (zip z (Tree color a xk x b)))
	   ((($ zipper 'Right color a xk x z) . b)  (zip z (Tree color a xk x b)))))
  
  ;; bbZip propagates a black deficit up the tree until either
  ;; the top is reached, or the deficit can be covered.  It
  ;; returns a boolean that is true if there is still a deficit
  ;; and the zipped tree.
  (define (bbZip zipper tree)
    (match (cons zipper tree)
	   ((($ zipper 'Top) . a)  (cons #t a))
	   ;; case 1L 
	   ((($ zipper 'Left 'Black xk x ($ tree 'Tree 'Red c yk y d) z) . a)
	    (bbZip (Left R xk x c (Left B yk y d z)) a))
	   ;; case 3L 
	   ((($ zipper 'Left color xk x ($ tree 'Tree 'Black ($ tree 'Tree 'Red c yk y d) wk w e) z) . a)
	    (bbZip (Left color xk x (Tree B c yk y (Tree R d wk w e)) z) a))
	   ;; case 4L 
	   ((($ zipper 'Left color xk x ($ tree 'Tree 'Black c yk y ($ tree 'Tree 'Red d wk w e)) z) . a)
	    (cons #f (zip z (Tree color (Tree B a xk x c) yk y (Tree B d wk w e)))))
	   ;; case 2L 
	   ((($ zipper 'Left 'Red xk x ($ tree 'Tree 'Black c yk y d) z) . a)
	    (cons #f (zip z (Tree B a xk x (Tree R c yk y d)))))
	   ;; case 2L 
	   ((($ zipper 'Left 'Black xk x ($ tree 'Tree 'Black c yk y d) z) . a)
	    (bbZip z (Tree B a xk x (Tree R c yk y d))))
	   ;; case 1R 
	   ((($ zipper 'Right color ($ tree 'Tree 'Red c yk y d) xk x z) . b) 
	    (bbZip (Right R d xk x (Right B c yk y z)) b))
	   ;; case 3R 
	   ((($ zipper 'Right color ($ tree 'Tree 'Black ($ tree 'Tree 'Red c wk w d) yk y e) xk x z) . b) 
	    (bbZip (Right color (Tree B c wk w (Tree R d yk y e)) xk x z) b))
	   ;; case 4R
	   ((($ zipper 'Right color ($ tree 'Tree 'Black c yk y ($ tree 'Tree 'Red d wk w e)) xk x z) . b) 
	    (cons #f (zip z (Tree color c yk y (Tree B (Tree R d wk w e) xk x b)))))
	   ;; case 2R 
	   ((($ zipper 'Right 'Red ($ tree 'Tree 'Black c yk y d) xk x z) . b) 
	    (cons #f (zip z (Tree B (Tree R c yk y d) xk x b))))
	   ;; case 2R 
	   ((($ zipper 'Right 'Black ($ tree 'Tree 'Black c yk y d) xk x z) . b) 
	    (bbZip z (Tree B (Tree R c yk y d) xk x b)))
	   (else   (cons #f (zip zipper tree)))))
  
  (define (delMin tree z)
    (match tree
	   (($ tree 'Tree 'Red ($ tree 'Empty) yk y b) 
	    (values yk y (cons #f (zip z b))))
	   (($ tree 'Tree 'Black ($ tree Empty) yk y b) 
	    (values yk y (bbZip z b)))
	   (($ tree 'Tree color a yk y b) 
	    (delMin a (Left color yk y b z)))
	   (($ tree 'Empty) (rb-tree:error 'delete "invalid tree"))))
  
  (define join
    (match-lambda* 
     (( 'Red ($ tree 'Empty) ($ tree 'Empty) z)  
      (zip z (Empty)))
     (( _ a ($ tree 'Empty) z)  
      (cdr  (bbZip z a)))
     (( _ ($ tree 'Empty) b z)
      (cdr  (bbZip z b)))
     ((color a b z)
      (let-values (((xk x b)  (delMin b (Top))))
        (match b
               ((#t . b1)  (cdr  (bbZip z (Tree color a xk x b1))))
               ((#f . b1)  (zip z (Tree color a xk x b1))))))))
  
  (define (del tree key z)
    (match tree 
	   (($ tree 'Empty)  #f)
	   (($ tree 'Tree color a yk y b)  
	    (dispatch-on-key key-compare 
	     key yk 
	     (del a key (Left color yk y b z))
	     (cons (cons yk y) (join color a b z))
	     (del b key (Right color a yk y z))))))

  (del root key (Top)))


(define (get-min root)
  (define (f root)
    (match root
	   (($ tree 'Empty)  #f)
	   (($ tree 'Tree _ _ ($ tree 'Empty) xk x _)  (cons xk x))
	   (($ tree 'Tree _ a _ _ _)   (f a))))
  (f root))


(define (get-max root)
  (define (f root)
    (match root
	   (($ tree 'Empty)  #f)
	   (($ tree 'Tree _ _ xk x ($ tree 'Empty))  (cons xk x))
	   (($ tree 'Tree _ _ _ _ b)   (f b))))
  (f root))


(define (foldv-limit root)
  (lambda (p f init) 
    (define (foldf tree ax)
      (match tree
	     (($ tree 'Empty)  ax)
	     (($ tree 'Tree _ a _ x b)  
	      (if (p ax) ax (foldf b (f x (foldf a ax)))))))
    (foldf root init)))


(define (foldv-right-limit root)
  (lambda (p f init) 
    (define (foldf tree ax)
      (match tree
	     (($ tree 'Empty)  ax)
	     (($ tree 'Tree _ a _ x b)  
	      (if (p ax) ax (foldf a (f x (foldf b ax)))))))
    (foldf root init)))


(define (foldv-partial root)
  (lambda (p f init) 
    (define (foldf tree ax)
      (match tree
	     (($ tree 'Empty)  ax)
	     (($ tree 'Tree _ a _ x b)  
	      (if (p x) (foldf b (f x (foldf a ax))) ax))))
    (foldf root init)))


(define (foldi-partial root)
  (lambda (p f init) 
    (define (foldf tree ax)
      (match tree
	     (($ tree 'Empty)  ax)
	     (($ tree 'Tree _ a xk x b)  
	      (if (p xk x) (foldf b (f xk x (foldf a ax))) ax))))
    (foldf root init)))


(define (foldv-right-partial root )
  (lambda (p f init) 
    (define (foldf tree ax)
      (match tree
	     (($ tree 'Empty)  ax)
	     (($ tree 'Tree _ a _ x b)  
	      (if (p x) (foldf a (f x (foldf b ax))) ax))))
    (foldf root init)))


(define (foldi-right-partial root)
  (lambda (p f init) 
    (define (foldf tree ax)
      (match tree
	     (($ tree 'Empty)  ax)
	     (($ tree 'Tree _ a xk x b)  
	      (if (p xk x) (foldf a (f xk x (foldf b ax))) ax))))
    (foldf root init)))


(define (foldv root)
  (lambda (f init)
    (define (foldf tree ax)
      (match tree
	     (($ tree 'Empty)  ax)
	     (($ tree 'Tree _ a _ x b)  (foldf b (f x (foldf a ax))))))
    (foldf root init)))


(define (foldi root)
  (lambda (f init)
    (define (foldf tree ax)
      (match tree
	     (($ tree 'Empty)  ax)
	     (($ tree 'Tree _ a xk x b)  (foldf b (f xk x (foldf a ax))))))
    (foldf root init)))


(define (foldv-right root)
  (lambda (f init)
    (define (foldf tree ax)
      (match tree
	     (($ tree 'Empty)  ax)
	     (($ tree 'Tree _ a _ x b)  (foldf a (f x (foldf b ax))))))
    (foldf root init)))


(define (foldi-right root)
  (lambda (f init)
    (define (foldf tree ax)
      (match tree
	     (($ tree 'Empty)  ax)
	     (($ tree 'Tree _ a xk x b)  (foldf a (f xk x (foldf b ax))))))
    (foldf root init)))



;; Returns an ordered list of the keys in the tree
(define (list-keys foldi-right)
  (foldi-right (lambda (k x l) (cons k l)) (list)))


;; Returns an ordered list of the (key . item) pairs in the tree
(define (list-items foldi-right)
  (foldi-right (lambda (k x l) (cons (cons k x) l)) (list)))


(define (for-each-ascending root )
  (define (appf f tree)
    (match tree
	   (($ tree 'Empty)  (void))
	   (($ tree 'Tree _ a k x b)  (begin (appf f a) (f (cons k x)) (appf f b)))))
  (lambda (f) (appf f root)))


(define (for-each-descending root)
  (define (appf f tree)
    (match tree
	   (($ tree 'Empty)  (void))
	   (($ tree 'Tree _ a k x b)  (begin (appf f b) (f (cons k x)) (appf f a)))))
  (lambda (f) (appf f root)))
  

(define (mapv root)
  (define (mapf f tree)
    (match tree
	   (($ tree 'Empty)  (Empty))
	   (($ tree 'Tree color a xk x b)  
	    (Tree color (mapf f a) xk (f x) (mapf f b)))))
  (lambda (f) (mapf f root)))


(define (mapi root)
  (define (mapf f tree)
    (match tree
	   (($ tree 'Empty)   (Empty))
	   (($ tree 'Tree color a xk x b)  
	    (Tree color (mapf f a) xk (f xk x) (mapf f b)))))
  (lambda (f) (mapf f root) ))


(define (apply-default-clause label key default-clause)
  (cond
   ((null? default-clause)
    (rb-tree:error label "key " key " was not found in the tree"))
   ((pair? (cdr default-clause))
    (rb-tree:error label "default argument must be a single clause"))
   ((procedure? (car default-clause)) ((car default-clause)))
   (else (car default-clause))))

;; functions for walking the tree while keeping a stack of parents to
;; be visited.


(define (next lst)
  (match lst
	 (((and t ($ tree 'Tree _ _ _ _ b)) . rest)  
	  (list t (left b rest)))
	 
	 (else (list (Empty) '()))))

(define (left t rest)
  (match t
	 (($ tree 'Empty) rest)
	 ((and t ($ tree 'Tree _ a _ _ _)) 
	  (left a (cons t rest)))))

(define (start t) (left t '()))


;; Support for constructing red-black trees in linear time from
;; increasing ordered sequences (based on a description by R. Hinze).
;; Note that the elements in the digits are ordered with the largest
;; on the left, whereas the elements of the trees are ordered with the
;; largest on the right.

(define-datatype digit digit?
  (Zero)
  (One (key identity)  (value identity) (tree tree?) (digit digit?))
  (Two (key1 identity) (value1 identity) (tree1 tree?)
       (key2 identity) (value2 identity) (tree2 tree?) (digit digit?)))


;; add an item that is guaranteed to be larger than any in l 
(define (add-item ak a l)
  (define (incr ak1 a1 t1 d)
    (match d
	   (($ digit 'Zero)
	    (One ak1 a1 t1 (Zero)))
	   (($ digit 'One ak2 a2 t2 r)
	    (Two ak1 a1 t1 ak2 a2 t2 r))
	   (($ digit 'Two ak2 a2 t2 ak3 a3 t3 r)
	    (One ak1 a1 t1 (incr ak2 a2 (Tree B t3 ak3 a3 t2) r)))))
  (incr ak a (Empty) l))


;; link the digits into a tree 
(define (link-all t)
  (define (link t d)
    (match d
	   (($ digit 'Zero)  t)
	   (($ digit 'One ak a t2 r) 
	    (link (Tree B t2 ak a t) r))
	   (($ digit 'Two ak1 a1 t1 ak2 a2 t2 r)
	    (link (Tree B (Tree R t2 ak2 a2 t1) ak1 a1 t) r))))
  (link (Empty) t))
	    
	   
(define (wrap f)
  (lambda (t1 t2)
    (match-let (((n result) (f (start t1) (start t2) 0 (Zero))))
               (link-all result))))

  
(define (map-insert t n result)
  (match t
	 ((($ tree 'Empty) _)  
	  (list n result))
	 ((($ tree 'Tree _ _ xk x _) r)
	  (map-insert (next r) (+ 1 n) (add-item xk x result)))))


;; Creates a map whose domain is the union of the domains of the two
;; input maps, using the supplied function to define the map on
;; elements that are in both domains.

(define (union-with merge-fn)
  (define (union k1 k2 key-compare)
    (lambda (t1 t2 n result)
      (let recur ((t1 t1) (t2 t2) (n n) (result result))
	(match (list (next t1) (next t2))
	       
	       (((($ tree 'Empty) _) (($ tree 'Empty) _))
		(list n result))
	       
	       (((($ tree 'Empty) _) t2)
		(map-insert t2 n result))
	       
	       ((t1 (($ tree 'Empty) _))
		(map-insert t1 n result))
	       
	       (((($ tree 'Tree _ _ xk x _) r1) (($ tree 'Tree _ _ yk y _) r2))
		(let ((xk1 (k1 xk)) (yk1 (k2 yk)))

		  (let ((c (key-compare xk1 yk1)))
		    (cond ((negative? c)   (recur r1 t2 (+ 1 n) (add-item xk1 x result)))
			  ((zero? c)       (recur r1 r2 (+ 1 n) (add-item xk1 (merge-fn x y) result)))
			  ((positive? c)   (recur t1 r2 (+ 1 n) (add-item yk1 y result)))))))
		
	       ))))
  (wrap union))


(define (union-withi merge-fn)
  (define (union k1 k2 key-compare)
    (lambda (t1 t2 n result)
      (let recur ((t1 t1) (t2 t2) (n n) (result result))
	(match (list (next t1) (next t2))
	       
	       (((($ tree 'Empty) _) (($ tree 'Empty) _))
		(list n result))
	       
	       (((($ tree 'Empty) _) t2)
		(map-insert t2 n result))
	       
	       ((t1 (($ tree 'Empty) _))
		(map-insert t1 n result))
	       
	       (((($ tree 'Tree _ _ xk x _) r1) (($ tree 'Tree_ _ yk y _) r2))
		(let ((xk1 (k1 xk)) (yk1 (k2 yk)))
		  (let ((c (key-compare xk1 yk1)))
		    (cond ((negative? c)   (recur r1 t2 (+ 1 n) (add-item xk1 x result)))
			  ((zero? c)       (recur r1 r2 (+ 1 n) (add-item xk1 (merge-fn xk1 x y) result)))
			  ((positive? c)   (recur t1 r2 (+ 1 n) (add-item yk1 y result)))))))
	       
	       ))))
    (wrap union))


(define (get-depth root)
  (let loop ((node root) (level 0))
    (match node 
           (($ tree 'Empty)  level)
           (($ tree 'Tree _ a _ _ b)  
            (max (loop a (+ 1 level))
                 (loop b (+ 1 level)))))))


(define (get-size root)
  (let loop ((node root))
    (match node 
           (($ tree 'Empty)  0)
           (($ tree 'Tree _ a _ _ b)  
            (+ 1 (loop a) (loop b)))
           ))
  )


(define (rb-tree-map key-compare #!key (insdel-key-compare key-compare) )

      (make-<PersistentMap>

       ;; empty tree
       (lambda () (Empty))

       ;; empty?
       (lambda (root)
         (cases tree root
                (Empty () #t)
                (else #f)))

       ;; size
       (lambda (root) 
         (get-size root))
       
       ;; get
       (lambda (root)
         (let ((find-assoc1 (find-assoc key-compare root)))
           (lambda (key . default-clause)
             (or (find-assoc1 key) 
                 (apply-default-clause 'get key default-clause)))))
	  
       ;; get-value
       (lambda (root)
         (let ((find-ref1 (find-ref key-compare root)))
           (lambda (key . default-clause)
             (or (find-ref1 key) (apply-default-clause 'get-value key default-clause)))))

       ;; get-min
       get-min
       
       ;; get-max
       get-max

       ;; list-keys
       (lambda (root)
         (list-keys (foldi-right root)))

       ;; list-items
       (lambda (root)
         (list-items (foldi-right root)))
	  
       ;; put
       (lambda (root key value)
         (let ((new-root (insert insdel-key-compare (lambda (x ax) x) root key value)))
           new-root))
	  
       ;; update
       (lambda (root key value merge-fn)
         (let ((new-root (insert insdel-key-compare merge-fn root key value)))
           new-root))
	  
       ;; delete
       (lambda (root key . default-clause)
         (or (let ((item+tree  (delete-assoc insdel-key-compare root key)))
               item+tree) 
             (apply-default-clause 'delete key default-clause)))
	  
       ;; for-each-ascending
       for-each-ascending

       ;; for-each-descending
       for-each-descending

       ;; map
       mapv

       ;; mapi
       mapi

       ;; fold
       foldv 

       ;; foldi
       foldi

       ;; fold-right
       foldv-right
       
       ;; foldi-right
       foldi-right
	  
       ;; fold-partial
       foldv-partial

       ;; foldi-partial
       foldi-partial 

       ;; fold-right-partial
       foldv-right-partial 

       ;; foldi-right-partial
       foldi-right-partial 
	  
       ;; fold-limit
       foldv-limit 

       ;; fold-right-limit
       foldv-right-limit 
       
       ))



)
