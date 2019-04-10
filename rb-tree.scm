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
;; Copyright 2007-2019 Ivan Raikov.
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
   for-each-ascending for-each-descending)


  (import scheme (chicken base)
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
;;		tree with the smallest key. If the tree is empty,
;;		an error is signalled.
;;
;;	get-max
;;
;;		returns a (key . value) pair for an association in the
;;		tree with the largest key. If the tree is empty, an
;;		error is signalled.
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
      (cases tree root
	   (Empty ()  (Tree R (Empty) key value (Empty)))
	   (Tree (color a yk y b)  
		 (dispatch-on-key key-compare 
		      key yk 
		      ;; Case 1: key < yk
		      (match a
			     (($ rb-tree#tree 'Tree 'Red c zk z d)
			      (dispatch-on-key key-compare
			       key zk
			       ;; Case 1.1: key < zk
			       (let ((c1 (ins c)))
                                 (match c1
                                        (($ rb-tree#tree 'Tree 'Red e wk w f)
                                         (Tree R (Tree B e wk w f) zk z (Tree B d yk y b)))
                                        (else  (Tree B (Tree R c1 zk z d) yk y b))))
			       ;; Case 1.2: key = zk
			       (Tree color (Tree R c key (value-merge value z) d) yk y b)
			       ;; Case 1.3: key > zk
			       (let ((d1 (ins d)))
                                 (match d1
                                        (($ rb-tree#tree 'Tree 'Red e wk w f)
                                         (Tree R (Tree B c zk z e)  wk  w  (Tree B f yk y b)))
                                        (else (Tree B (Tree R c zk z d1) yk y b))))))
			     (else (let ((a1  (ins a)))
                                     (Tree B a1 yk y b))))
		      ;; Case 2: key  = yk
		      (Tree color a key (value-merge value y) b)
		      ;; Case 3: key  > yk
		      (match b
			     (($ rb-tree#tree 'Tree 'Red c zk z d)
			      (dispatch-on-key key-compare
			       key zk
			       ;; Case 3.1: key < zk
			       (let ((c1 (ins c)))
                                 (match c1
                                        (($ rb-tree#tree 'Tree 'Red e wk w f)
                                         (Tree R (Tree B a yk y e)  wk  w (Tree B f zk z d)))
                                        (else (Tree B a yk y (Tree R c1 zk z d)))))
			       ;; Case 3.2: key = zk
			       (Tree color a yk y (Tree R c key (value-merge value z) d))
			       ;; Case 3.3: key > zk
			       (let ((d1 (ins d)))
                                 (match d1
                                        (($ rb-tree#tree 'Tree 'Red e wk w f)
                                         (Tree R (Tree B a yk y c)  zk z (Tree B e wk w f)))
                                        (else (Tree B a yk y (Tree R c zk z d1)))))))
			     (else (let ((b1 (ins b)))
				     (Tree B a yk y b1))))))))
    )


;; Looks for an item: Given a key, returns the corresponding (key
;; . value) association or #f if the tree does not contain an
;; association with that key.
(define (find-assoc key-compare root key)
    (let recur ((root root))
      (cases tree root 
	     (Empty ()  #f)
	     (Tree (c a yk y b)
		   (dispatch-on-key key-compare 
				    key yk (recur a) (cons yk y) (recur b)))))
    )

;; Looks for an item: Given a key, returns the value of the
;; corresponding (key . value) association or #f if the tree does
;; not contain an association with that key.
(define (find-ref key-compare root key)
    (let recur ((root root))
      (cases tree root 
	     (Empty ()  #f)
	     (Tree (c a yk y b)
		   (dispatch-on-key key-compare 
				    key yk (recur a) y (recur b)))))
    )

;; Finds an association with a given key, and deletes it.  Returns
;; the (key . value) pair of the deleted association, or #f if it
;; couldn't be found

(define (delete-assoc key-compare root key)

  (define (zip zipper tree)
    (match (cons zipper tree)
	   ((($ rb-tree#zipper 'Top) . a)  tree)
	   ((($ rb-tree#zipper 'Left color xk x b z) . a)   (zip z (Tree color a xk x b)))
	   ((($ rb-tree#zipper 'Right color a xk x z) . b)  (zip z (Tree color a xk x b)))))
  
  ;; bbZip propagates a black deficit up the tree until either
  ;; the top is reached, or the deficit can be covered.  It
  ;; returns a boolean that is true if there is still a deficit
  ;; and the zipped tree.
  (define (bbZip zipper tree)
    (match (cons zipper tree)
	   ((($ rb-tree#zipper 'Top) . a)  (cons #t a))
	   ;; case 1L 
	   ((($ rb-tree#zipper 'Left 'Black xk x ($ rb-tree#tree 'Tree 'Red c yk y d) z) . a)
	    (bbZip (Left R xk x c (Left B yk y d z)) a))
	   ;; case 3L 
	   ((($ rb-tree#zipper 'Left color xk x ($ rb-tree#tree 'Tree 'Black ($ rb-tree#tree 'Tree 'Red c yk y d) wk w e) z) . a)
	    (bbZip (Left color xk x (Tree B c yk y (Tree R d wk w e)) z) a))
	   ;; case 4L 
	   ((($ rb-tree#zipper 'Left color xk x ($ rb-tree#tree 'Tree 'Black c yk y ($ rb-tree#tree 'Tree 'Red d wk w e)) z) . a)
	    (cons #f (zip z (Tree color (Tree B a xk x c) yk y (Tree B d wk w e)))))
	   ;; case 2L 
	   ((($ rb-tree#zipper 'Left 'Red xk x ($ rb-tree#tree 'Tree 'Black c yk y d) z) . a)
	    (cons #f (zip z (Tree B a xk x (Tree R c yk y d)))))
	   ;; case 2L 
	   ((($ rb-tree#zipper 'Left 'Black xk x ($ rb-tree#tree 'Tree 'Black c yk y d) z) . a)
	    (bbZip z (Tree B a xk x (Tree R c yk y d))))
	   ;; case 1R 
	   ((($ rb-tree#zipper 'Right color ($ rb-tree#tree 'Tree 'Red c yk y d) xk x z) . b) 
	    (bbZip (Right R d xk x (Right B c yk y z)) b))
	   ;; case 3R 
	   ((($ rb-tree#zipper 'Right color ($ rb-tree#tree 'Tree 'Black ($ rb-tree#tree 'Tree 'Red c wk w d) yk y e) xk x z) . b) 
	    (bbZip (Right color (Tree B c wk w (Tree R d yk y e)) xk x z) b))
	   ;; case 4R
	   ((($ rb-tree#zipper 'Right color ($ rb-tree#tree 'Tree 'Black c yk y ($ rb-tree#tree 'Tree 'Red d wk w e)) xk x z) . b) 
	    (cons #f (zip z (Tree color c yk y (Tree B (Tree R d wk w e) xk x b)))))
	   ;; case 2R 
	   ((($ rb-tree#zipper 'Right 'Red ($ rb-tree#tree 'Tree 'Black c yk y d) xk x z) . b) 
	    (cons #f (zip z (Tree B (Tree R c yk y d) xk x b))))
	   ;; case 2R 
	   ((($ rb-tree#zipper 'Right 'Black ($ rb-tree#tree 'Tree 'Black c yk y d) xk x z) . b) 
	    (bbZip z (Tree B (Tree R c yk y d) xk x b)))
	   (else   (cons #f (zip zipper tree)))))
  
  (define (delMin tree z)
    (match tree
	   (($ rb-tree#tree 'Tree 'Red ($ rb-tree#tree 'Empty) yk y b) 
	    (values yk y (cons #f (zip z b))))
	   (($ rb-tree#tree 'Tree 'Black ($ rb-tree#tree Empty) yk y b) 
	    (values yk y (bbZip z b)))
	   (($ rb-tree#tree 'Tree color a yk y b) 
	    (delMin a (Left color yk y b z)))
	   (($ rb-tree#tree 'Empty) (rb-tree:error 'delete "invalid tree"))))
  
  (define join
    (match-lambda* 
     (( 'Red ($ rb-tree#tree 'Empty) ($ rb-tree#tree 'Empty) z)  
      (zip z (Empty)))
     (( _ a ($ rb-tree#tree 'Empty) z)  
      (cdr  (bbZip z a)))
     (( _ ($ rb-tree#tree 'Empty) b z)
      (cdr  (bbZip z b)))
     ((color a b z)
      (let-values (((xk x b)  (delMin b (Top))))
        (match b
               ((#t . b1)  (cdr  (bbZip z (Tree color a xk x b1))))
               ((#f . b1)  (zip z (Tree color a xk x b1))))))))
  
  (define (del tree key z)
    (match tree 
	   (($ rb-tree#tree 'Empty)  #f)
	   (($ rb-tree#tree 'Tree color a yk y b)  
	    (dispatch-on-key key-compare 
	     key yk 
	     (del a key (Left color yk y b z))
	     (cons (cons yk y) (join color a b z))
	     (del b key (Right color a yk y z))))))

  (del root key (Top)))


(define (get-min root)
  (define (f root)
    (match root
	   (($ rb-tree#tree 'Empty)  #f)
	   (($ rb-tree#tree 'Tree _ _ ($ rb-tree#tree 'Empty) xk x _)  (cons xk x))
	   (($ rb-tree#tree 'Tree _ a _ _ _)   (f a))))
  (f root))


(define (get-max root)
  (define (f root)
    (match root
	   (($ rb-tree#tree 'Empty)  #f)
	   (($ rb-tree#tree 'Tree _ _ xk x ($ rb-tree#tree 'Empty))  (cons xk x))
	   (($ rb-tree#tree 'Tree _ _ _ _ b)   (f b))))
  (f root))



(define (for-each-ascending root f)
  (define (appf f tree)
    (match tree
	   (($ rb-tree#tree 'Empty)  (void))
	   (($ rb-tree#tree 'Tree _ a k x b)  (begin (appf f a) (f (cons k x)) (appf f b)))))
  (appf f root))


(define (for-each-descending root f)
  (define (appf f tree)
    (match tree
	   (($ rb-tree#tree 'Empty)  (void))
	   (($ rb-tree#tree 'Tree _ a k x b)  (begin (appf f b) (f (cons k x)) (appf f a)))))
  (appf f root))
  



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
	 (((and t ($ rb-tree#tree 'Tree _ _ _ _ b)) . rest)  
	  (list t (left b rest)))
	 
	 (else (list (Empty) '()))))

(define (left t rest)
  (match t
	 (($ rb-tree#tree 'Empty) rest)
	 ((and t ($ rb-tree#tree 'Tree _ a _ _ _)) 
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
	 ((($ rb-tree#tree 'Empty) _)  
	  (list n result))
	 ((($ rb-tree#tree 'Tree _ _ xk x _) r)
	  (map-insert (next r) (+ 1 n) (add-item xk x result)))))


;; Creates a map whose domain is the union of the domains of the two
;; input maps, using the supplied function to define the map on
;; elements that are in both domains.

(define (union-with merge-fn)
  (define (union k1 k2 key-compare)
    (lambda (t1 t2 n result)
      (let recur ((t1 t1) (t2 t2) (n n) (result result))
	(match (list (next t1) (next t2))
	       
	       (((($ rb-tree#tree 'Empty) _) (($ rb-tree#tree 'Empty) _))
		(list n result))
	       
	       (((($ rb-tree#tree 'Empty) _) t2)
		(map-insert t2 n result))
	       
	       ((t1 (($ rb-tree#tree 'Empty) _))
		(map-insert t1 n result))
	       
	       (((($ rb-tree#tree 'Tree _ _ xk x _) r1) (($ rb-tree#tree 'Tree _ _ yk y _) r2))
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
	       
	       (((($ rb-tree#tree 'Empty) _) (($ rb-tree#tree 'Empty) _))
		(list n result))
	       
	       (((($ rb-tree#tree 'Empty) _) t2)
		(map-insert t2 n result))
	       
	       ((t1 (($ rb-tree#tree 'Empty) _))
		(map-insert t1 n result))
	       
	       (((($ rb-tree#tree 'Tree _ _ xk x _) r1) (($ rb-tree#tree 'Tree_ _ yk y _) r2))
		(let ((xk1 (k1 xk)) (yk1 (k2 yk)))
		  (let ((c (key-compare xk1 yk1)))
		    (cond ((negative? c)   (recur r1 t2 (+ 1 n) (add-item xk1 x result)))
			  ((zero? c)       (recur r1 r2 (+ 1 n) (add-item xk1 (merge-fn xk1 x y) result)))
			  ((positive? c)   (recur t1 r2 (+ 1 n) (add-item yk1 y result)))))))
	       
	       ))))
    (wrap union))

	   
(define (wrap-transform f)
  (lambda (t1 t2)
    (match-let (((n result kt1 kt2) (f (start t1) (start t2) 0 (Zero) '() '())))
               (list n (link-all result) kt1 kt2))))

  
(define (map-transform f t n result kt)
  (match t
	 ((($ rb-tree#tree 'Empty) _)  
	  (list n result kt))
	 ((($ rb-tree#tree 'Tree _ _ xk x _) r)
          (let ((x1 (f x)))
            (map-transform f (next r) (+ 1 n) (add-item xk x1 result) (cons (cons xk x1) kt))))
         ))


(define (union-transform fn merge-fn)
  (define (union kt1 kt2 key-compare)
    (lambda (t1 t2 n result)
      (let recur ((t1 t1) (t2 t2) (n n) (result result) (kt1 kt1) (kt2 kt2))
	(match (list (next t1) (next t2))
	       
	       (((($ rb-tree#tree 'Empty) _) (($ rb-tree#tree 'Empty) _))
		(list n result kt1 kt2))
	       
	       (((($ rb-tree#tree 'Empty) _) t2)
		(match-let (((n result kt2) (map-transform fn t2 n result kt2)))
                           (list n result kt1 kt2)))
	       
	       ((t1 (($ rb-tree#tree 'Empty) _))
		(match-let (((n result kt1) (map-transform fn t1 n result kt1)))
                           (list result kt1 kt2)))
	       
	       (((($ rb-tree#tree 'Tree _ _ xk x _) r1) (($ rb-tree#tree 'Tree _ _ yk y _) r2))
		(let ((xk1 (kt1 xk)) (yk1 (kt2 yk)))

		  (let ((c (key-compare xk1 yk1)))
		    (cond ((negative? c)
                           (let ((x1 (fn x)))
                             (recur r1 t2 (+ 1 n) (add-item xk1 x1 result) (cons (cons xk1 x1) kt1) kt2)))
			  ((zero? c)
                           (let ((x1 (merge-fn x y)))
                             (recur r1 r2 (+ 1 n) (add-item xk1 x1 result) (cons (cons xk1 x1) kt1) kt2)))
			  ((positive? c)
                           (let ((y1 (fn y)))
                             (recur t1 r2 (+ 1 n) (add-item yk1 y1 result) kt1 (cons (cons yk1 y1) kt2))))
                          ))
                  ))
		
	       ))
      ))
  (wrap-transform union))


(define (get-depth root)
  (let loop ((node root) (level 0))
    (match node 
           (($ rb-tree#tree 'Empty)  level)
           (($ rb-tree#tree 'Tree _ a _ _ b)  
            (max (loop a (+ 1 level))
                 (loop b (+ 1 level)))))))


(define (get-size root)
  (let loop ((node root))
    (match node 
           (($ rb-tree#tree 'Empty) 0)
           (($ rb-tree#tree 'Tree _ a _ _ b)
            (+ 1 (loop a) (loop b)))
           ))
  )

(define *eof-object* (read (open-input-string "")))
(define (eof-object) *eof-object*)

(define-record-type <root>
  (make-root val)
  root?
  (val root-val root-val-set!))


(define (value-generator root)
  (let ((s (make-root
            (match (start root)
                   ((fst . rst) 
                    (cons fst rst))
                   (else '())))))
    (lambda ()
      (let ((sval (root-val s)))
        (if (null? sval)
            (eof-object)
            (match-let (((t sval-next) (next sval)))
                       (root-val-set! s sval-next)
                       (cases tree t
                              (Empty () (eof-object))
                              (Tree (c l k v r) v))
                       ))
        ))
    ))


(define (key-generator root)
  (let ((s (make-root
            (match (start root)
                   ((fst . rst)
                    (cons fst rst))
                   (else '())))))
    (lambda ()
      (let ((sval (root-val s)))
        (if (null? sval)
            (eof-object)
            (match-let (((t sval-next) (next sval)))
                       (root-val-set! s sval-next)
                       (cases tree t
                              (Empty () (eof-object))
                              (Tree (c l k v r) k))
                       ))
        ))
    ))




(define (rb-tree-map key-compare #!key
                     (new-root (Empty)))
  (let ( (root new-root) )
    (object
      ;; persistent map behaviors
      ((persistent-map? self) #t)
      ((size self) (get-size root))
      
      ((empty? self)
       (cases tree root
              (Empty () #t)
              (else #f)))
      
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
        (get-min root))
       
       ((get-max self)
        (get-max root))
	  
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
        (or (let* ((item+tree  (delete-assoc key-compare root key))
                   (item (car item+tree))
                   (new-root (cdr item+tree)))
              (cons item (rb-tree-map key-compare new-root: new-root)))
            (apply-default-clause 'delete key '())))
       
       ((for-each-ascending self f)
        (for-each-ascending root f))
       
       ((for-each-descending self f)
        (for-each-descending root f))

       ;; collection behaviors
       ((collection? self) #t)
       ((gen-keys self) (key-generator root))
       ((gen-elts self) (value-generator root))
       ((for-each-key self proc)
        (for-each-ascending root (lambda (item) (proc (car item)))))
       ((for-each-elt self proc)
        (for-each-ascending root (lambda (item) (proc (cdr item)))))
        
       ))
  )


)
