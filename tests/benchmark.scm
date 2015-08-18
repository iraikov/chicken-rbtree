(require-extension typeclass srfi-1 rb-tree)
(use data-structures extras utils)

(define (++ x) (fx+ 1 x))
(define (-- x) (fx- x 1))

(define min-key 1) 
(define max-key 1000000)

(define (key-compare x y) (- x y))

(define compute-assoc (lambda (key) (cons key (++ key))))

(define (new-map) (rb-tree-map key-compare))
	   

(let ((m (new-map)))
  (with-instance ((<PersistentMap> m))

    (let ((t
           (time
            (let recur  ((t (empty)) (i min-key))
              (let ((t1 (put t i (cdr (compute-assoc i)))))
                (if (< i max-key) (recur t1 (++ i)) t1))))))
      t)))

            

