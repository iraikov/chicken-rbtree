
;;
;; Verifying the rb-tree package
;;

(import scheme (chicken base) (chicken format) typeclass srfi-1 rb-tree test)

(define (++ x) (+ 1 x))
(define (-- x) (- x 1))

(define min-key 1) 
(define max-key 10000)

(define (key-compare x y) (- x y))

(define (new-map) (rb-tree-map key-compare))
	   
;; a hard-wired association between a key and a value"   
(define compute-assoc (lambda (key) (cons key (++ key))))
	   
(test-group "persistent map"

  (test-group "the empty and size predicates on an empty map"

   (let ((m (new-map)))
     (with-instance ((<PersistentMap> m))
       (let ((t (empty)))
       (test-assert  (empty? t))
       (test-assert (zero? (size t)))
       ))
     ))

  (test-group (sprintf "loading a sequence [~A,~A] in ascending order" min-key max-key)
   (let ((m (new-map)))
     (with-instance ((<PersistentMap> m))
      (let ((t 
             (time
              (let recur  ((t (empty)) (i min-key))
                (let ((t1 (put t i (cdr (compute-assoc i)))))
                  (test (sprintf "get element ~A" i) (compute-assoc i) ((get t1) i))
                  (if (< i max-key) (recur t1 (++ i)) t1))))))
            
         (test (++ (- max-key min-key)) (size t))
         (test-assert (not (empty? t)))
         
         (let ((gett (get t)))
           (test (compute-assoc (++ min-key)) (gett (++ min-key)))
           (test (compute-assoc (++ min-key)) (gett (++ min-key) #f))
         
           (test-assert "looking up of non-existing keys" 
                        (not (gett (-- min-key) #f)))
           )
       ))
     ))

  (test-group "reloading the same sequence in descending order and then deleting" 
	      
   (let ((m (new-map)))
     (with-instance ((<PersistentMap> m))
      (let ((t 
             (time
              (let recur  ((t (empty)) (i max-key))
                (let ((t1 (put t i (cdr (compute-assoc i)))))
                  (test (sprintf "get element ~A" i) (compute-assoc i) ((get t1) i))
                  (let ((t2 (delete t1 i)))
                    (if (< min-key i) (recur (cdr t2) (- i 1)) (cdr t2)))))))
            )

        (test-assert (zero? (size t))))
      ))
   )

  (test-group "fold and map"
   (let ((m (new-map)))
     (with-instance ((<PersistentMap> m))
      (let ((t
             (let recur  ((t (empty)) (i min-key))
                   (let ((t1 (put t i (cdr (compute-assoc i)))))
                     (if (< i max-key) (recur t1 (++ i)) t1)))))

	(test "using fold to sum the elements in the persistent-map"  
              (* 5000 (+ (+ 1 min-key) (+ 1 max-key)))
              ((fold t) (lambda (x sum) (+ x sum)) 0))

         (test-group "using map to multiply each elements by 10"
                     (let (
                           (t-x10 ((map t) (lambda (x) (* x 10))))
                           (compute-assoc-x10 (lambda (key) (cons key (* 10 (++ key)))))
                           )
                         (do ((i min-key (++ i))) ((> i max-key))
                           (test (sprintf "element ~A" i)
                                 (compute-assoc-x10 i) 
                                 ((get t-x10) i) ))))))
       ))
   )

(test-exit)


