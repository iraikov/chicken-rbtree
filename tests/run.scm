
;;
;; Verifying the rb-tree package
;;

(import scheme (chicken base) (chicken format) srfi-1 rb-tree yasos-collections test)

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
     (test-assert (empty? m))
     (test-assert (zero? (size m)))
     ))

  (test-group (sprintf "loading a sequence [~A,~A] in ascending order" min-key max-key)
   (let ((m (new-map)))
      (let ((m1
             (time
              (let recur  ((m m) (i min-key))
                (let ((m1 (put m i (cdr (compute-assoc i)))))
                  (test (sprintf "get element ~A" i) (compute-assoc i) (get m1 i))
                  (if (< i max-key) (recur m1 (++ i)) m1))))))
            
         (test (++ (- max-key min-key)) (size m1))
         (test-assert (not (empty? m1)))
         
         (test (compute-assoc (++ min-key)) (get m1 (++ min-key)))
         (test (compute-assoc (++ min-key)) (get/default m1 (++ min-key) #f))
         
         (test-assert "looking up of non-existing keys" 
                      (not (get/default m1 (-- min-key) #f)))
       ))
     )

  (test-group (sprintf "generating a sequence [~A,~A]" min-key max-key)
   (let ((m (new-map)))
      (let ((m1
             (time
              (generate m (lambda (x) (> x max-key))
                        (lambda (x) (compute-assoc x))
                        (lambda (x) (++ x))
                        min-key))))
            
         (test (++ (- max-key min-key)) (size m1))
         (test-assert (not (empty? m1)))
         
         (test (compute-assoc (++ min-key)) (get m1 (++ min-key)))
         (test (compute-assoc (++ min-key)) (get/default m1 (++ min-key) #f))
       ))
     )

  (test-group "reloading the same sequence in descending order and then deleting" 
	      
   (let ((m (new-map)))
      (let ((m1 
             (time
              (let recur  ((m m) (i max-key))
                (let ((m1 (put m i (cdr (compute-assoc i)))))
                  (test (sprintf "get element ~A" i) (compute-assoc i) (get m1 i))
                  (let ((m2 (delete m1 i)))
                    (if (< min-key i) (recur (cdr m2) (- i 1)) (cdr m2)))))))
            )

        (test-assert (zero? (size m1))))
      ))

  (test-group "reduce and map"
    (let ((m
           (let recur  ((m (new-map)) (i min-key))
             (let ((m1 (put m i (cdr (compute-assoc i)))))
               (if (< i max-key) (recur m1 (++ i)) m1)))))

	(test "using reduce to sum the elements in the persistent-map"  
              (* 5000 (+ (+ 1 min-key) (+ 1 max-key)))
              (reduce (lambda (x sum) (+ x sum)) 0 m))

         (test-group "using map to multiply each elements by 10"
                     (let (
                           (m-x10 (map-elts (lambda (x) (* x 10)) m))
                           (compute-assoc-x10 (lambda (key) (cons key (* 10 (++ key)))))
                           )
                       (print m-x10)
                         (do ((i min-key (++ i))) ((>= i max-key))
                           (test (sprintf "element ~A" i)
                                 (cdr (compute-assoc-x10 i))
                                 (elt-ref m-x10 (-- i) ))))))
       )
   )

(test-exit)


