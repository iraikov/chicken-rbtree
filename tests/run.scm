
;;
;; Verifying the rb-tree package
;;

(import scheme (chicken base) (chicken format) (prefix (chicken sort) s:) srfi-1 rb-tree yasos-collections test)

(define (generator->list g)
  (let loop ((acc '()))
    (let ((x (g)))
      (if (eof-object? x) (reverse acc) (loop (cons x acc))))))

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

  (test-group "minimum, maximum and traversals on an empty map"
    (let ((m (new-map)))
      (test #f (get-min m))
      (test #f (get-max m))
      (test '() (let ((acc '())) (for-each-ascending m (lambda (kv) (set! acc (cons kv acc)))) acc))
      (test '() (let ((acc '())) (for-each-descending m (lambda (kv) (set! acc (cons kv acc)))) acc))
      (test '() (generator->list (gen-range m #f #f)))))

  (test-group "minimum, maximum and traversals"
    (let ((m (fold (lambda (k m) (put m k (++ k))) (new-map) '(5 3 9 1 7 2 8))))
      (test (compute-assoc 1) (get-min m))
      (test (compute-assoc 9) (get-max m))
      (test (map compute-assoc '(1 2 3 5 7 8 9))
            (let ((acc '())) (for-each-descending m (lambda (kv) (set! acc (cons kv acc)))) acc))
      (test (map compute-assoc '(9 8 7 5 3 2 1))
            (let ((acc '())) (for-each-ascending m (lambda (kv) (set! acc (cons kv acc)))) acc))
      (test '(1 2 3 5 7 8 9)
            (let ((acc '())) (for-each-key m (lambda (k) (set! acc (cons k acc)))) (reverse acc)))))

  (test-group "range operations"
    (let ((m (fold (lambda (k m) (put m k (++ k))) (new-map) (iota 50 0 2))))
      (define (range-keys lo hi)
        (let ((acc '()))
          (for-each-range m lo hi (lambda (kv) (set! acc (cons (car kv) acc))))
          (reverse acc)))
      (define (model-keys lo hi)
        (filter (lambda (k) (and (or (not lo) (>= k lo)) (or (not hi) (< k hi))))
                (iota 50 0 2)))
      (for-each
       (lambda (lo+hi)
         (let ((lo (car lo+hi)) (hi (cadr lo+hi)))
           (test (sprintf "for-each-range [~A, ~A)" lo hi) (model-keys lo hi) (range-keys lo hi))
           (test (sprintf "gen-range [~A, ~A)" lo hi)
                 (map compute-assoc (model-keys lo hi))
                 (generator->list (gen-range m lo hi)))))
       '((#f #f) (10 20) (11 21) (-5 3) (97 200) (98 #f) (#f 1) (#f 0) (30 30) (40 31)))))

  (test-group "random puts and deletes against a sorted list model"
    (define (next-seed s) (modulo (+ (* s 1103515245) 12345) 2147483648))
    (define (cmp x y) (cond ((< x y) -1) ((> x y) 1) (else 0)))
    (define (model-put lst k)
      (cond ((null? lst) (list k))
            ((< k (car lst)) (cons k lst))
            ((= k (car lst)) lst)
            (else (cons (car lst) (model-put (cdr lst) k)))))
    (define (check nsteps nkeys)
      (let loop ((i 0) (s 7) (m (rb-tree-map cmp)) (model '()) (ok #t))
        (if (or (= i nsteps) (not ok))
            (begin (unless ok (printf "mismatch at step ~A~%" i)) ok)
            (let* ((s (next-seed s))
                   (k (modulo (quotient s 7) nkeys)))
              (receive (m model)
                  (cond ((even? (quotient s 3))
                         (values (put m k (* 10 k)) (model-put model k)))
                        ((memv k model)
                         (values (cdr (delete m k)) (delete! k model)))
                        (else (values m model)))
                (loop (+ i 1) s m model
                      (and (equal? model (generator->list (gen-keys m)))
                           (= (length model) (size m))
                           (rb-tree-map-valid? m)
                           (every (lambda (k) (= (* 10 k) (get-value m k))) model))))))))
    (test-assert "2000 steps over keys 0-39" (check 2000 40))
    (test-assert "5000 steps over keys 0-299" (check 5000 300)))

  (test-group "unions"
    (define (alist->map alist)
      (fold (lambda (kv m) (put m (car kv) (cdr kv))) (new-map) alist))
    (define (map->alist m)
      (let ((acc '())) (for-each-ascending m (lambda (kv) (set! acc (cons kv acc)))) (reverse acc)))
    (define (check-map name expected m)
      (test (string-append name ": associations") expected (map->alist m))
      (test (string-append name ": size") (length expected) (size m))
      (test-assert (string-append name ": red-black invariants") (rb-tree-map-valid? m)))

    (define odds  (map (lambda (k) (cons k (* 10 k))) (iota 20 1 2)))  ; 1 3 .. 39
    (define evens (map (lambda (k) (cons k (* 10 k))) (iota 20 0 2)))  ; 0 2 .. 38
    (define low   (map (lambda (k) (cons k k)) (iota 10 0)))           ; 0 .. 9
    (define mid   (map (lambda (k) (cons k (- k))) (iota 10 5)))       ; 5 .. 14
    (define (sort-alist l) (s:sort l (lambda (a b) (< (car a) (car b)))))

    (test-group "union-with"
      (check-map "disjoint" (sort-alist (append odds evens))
                 ((union-with +) (alist->map odds) (alist->map evens)))
      (check-map "overlapping"
                 (append (map (lambda (k) (cons k k)) (iota 5 0))
                         (map (lambda (k) (cons k 0)) (iota 5 5))
                         (map (lambda (k) (cons k (- k))) (iota 5 10)))
                 ((union-with +) (alist->map low) (alist->map mid)))
      (check-map "empty left" mid ((union-with +) (new-map) (alist->map mid)))
      (check-map "empty right" low ((union-with +) (alist->map low) (new-map)))
      (check-map "both empty" '() ((union-with +) (new-map) (new-map)))
      (check-map "identical" (map (lambda (kv) (cons (car kv) (* 2 (cdr kv)))) low)
                 ((union-with +) (alist->map low) (alist->map low)))
      (let* ((m1 (alist->map low)) (m2 (alist->map mid))
             (u ((union-with +) m1 m2)))
        (test "inputs unchanged" (list low mid) (list (map->alist m1) (map->alist m2)))
        (test "result supports put" (cons 100 1) (get (put u 100 1) 100))
        (test "result supports delete" 14
              (size (cdr (delete u 7))))))

    (test-group "union-withi"
      (check-map "overlapping"
                 (append (map (lambda (k) (cons k k)) (iota 5 0))
                         (map (lambda (k) (cons k (list k k (- k)))) (iota 5 5))
                         (map (lambda (k) (cons k (- k))) (iota 5 10)))
                 ((union-withi list) (alist->map low) (alist->map mid)))
      (check-map "disjoint" (sort-alist (append odds evens))
                 ((union-withi list) (alist->map odds) (alist->map evens)))
      (check-map "empty left" low ((union-withi list) (new-map) (alist->map low))))

    (test-group "union-transform"
      ;; values are positions in the input data; FN numbers the result
      ;; positions in ascending order of keys, as dataframe merge does
      (define (run alist1 alist2)
        (let ((counter 0))
          ((union-transform
            (lambda (x) (let ((c counter)) (set! counter (+ 1 c)) c))
            (lambda (x y) (let ((c counter)) (set! counter (+ 1 c)) c)))
           (alist->map alist1) (alist->map alist2))))
      (define pos1 (map (lambda (k i) (cons k i)) '(1 3 5 7) (iota 4)))
      (define pos2 (map (lambda (k i) (cons k i)) '(3 4 7 9 10) (iota 5 100)))
      (let ((r (run pos1 pos2)))
        (test "overlapping: n" 7 (list-ref r 0))
        (check-map "overlapping" (map (lambda (k i) (cons k i)) '(1 3 4 5 7 9 10) (iota 7))
                   (list-ref r 1))
        (test "overlapping: kt1" '((0 . 0) (1 . 1) (2 . 3) (3 . 4)) (list-ref r 2))
        (test "overlapping: kt2" '((100 . 1) (101 . 2) (102 . 4) (103 . 5) (104 . 6))
              (list-ref r 3)))
      (let ((r (run pos1 '())))
        (test "empty right: n" 4 (list-ref r 0))
        (check-map "empty right" (map (lambda (k i) (cons k i)) '(1 3 5 7) (iota 4)) (list-ref r 1))
        (test "empty right: kt1" '((0 . 0) (1 . 1) (2 . 2) (3 . 3)) (list-ref r 2))
        (test "empty right: kt2" '() (list-ref r 3)))
      (let ((r (run '() pos2)))
        (test "empty left: n" 5 (list-ref r 0))
        (test "empty left: kt1" '() (list-ref r 2))
        (test "empty left: kt2" '((100 . 0) (101 . 1) (102 . 2) (103 . 3) (104 . 4)) (list-ref r 3)))
      (let ((r (run '() '())))
        (test "both empty" '(0 () ()) (list (list-ref r 0) (list-ref r 2) (list-ref r 3)))
        (check-map "both empty" '() (list-ref r 1)))
      (let ((r (run (map (lambda (k) (cons k k)) (iota 10 0 2))
                    (map (lambda (k) (cons k k)) (iota 10 1 2)))))
        (test "disjoint: n" 20 (list-ref r 0))
        (check-map "disjoint" (map (lambda (k) (cons k k)) (iota 20)) (list-ref r 1))))

    (test-group "large unions keep the red-black invariants"
      (for-each
       (lambda (n1 n2)
         (let* ((a1 (map (lambda (k) (cons k k)) (iota n1 0 3)))
                (a2 (map (lambda (k) (cons k k)) (iota n2 0 2)))
                (u ((union-with (lambda (x y) x)) (alist->map a1) (alist->map a2)))
                (keys (delete-duplicates (s:sort (append (map car a1) (map car a2)) <))))
           (test (sprintf "union of ~A and ~A keys" n1 n2) keys (map car (map->alist u)))
           (test-assert (sprintf "invariants for ~A and ~A keys" n1 n2) (rb-tree-map-valid? u))))
       '(1 2 3 7 100 1000 1 0)
       '(1 1 5 8 333 1    1000 1023))))
   )

(test-exit)
