#lang scheme/gui

(require (planet williams/science/random-distributions))

(define-struct statistics
  (initial-size
   (n #:mutable)
   (data #:mutable)
   (sum #:mutable)
   (xmin #:mutable)
   (x.25 #:mutable)
   (x.5 #:mutable)
   (x.75 #:mutable)
   (xmax #:mutable)
   (range #:mutable)
   (IQR #:mutable)))

(define (new-statistics (initial-size 8))
  (make-statistics
   initial-size
   0
   (make-vector initial-size)
   0
   +inf.0
   +nan.0
   +nan.0
   +nan.0
   -inf.0
   +nan.0
   +nan.0))

(define (statistics-mean statistics)
  (/ (statistics-sum statistics)
     (statistics-n statistics)))

(define (statistics-inner-fence statistics)
  (values (- (statistics-x.25 statistics)
             (* 1.5 (statistics-IQR statistics)))
          (+ (statistics-x.75 statistics)
             (* 1.5 (statistics-IQR statistics)))))

(define (statistics-outer-fence statistics)
  (values (- (statistics-x.25 statistics)
             (* 3.0 (statistics-IQR statistics)))
          (+ (statistics-x.75 statistics)
             (* 3.0 (statistics-IQR statistics)))))

(define (statistics-reset! statistics)
  (set-statistics-n! statistics 0)
  (set-statistics-data! statistics (make-vector (statistics-initial-size statistics)))
  (set-statistics-sum! statistics 0)
  (set-statistics-xmin! statistics +inf.0)
  (set-statistics-x.25! statistics +nan.0)
  (set-statistics-x.5! statistics +nan.0)
  (set-statistics-x.75! statistics +nan.0)
  (set-statistics-xmax! statistics -inf.0)
  (set-statistics-range! statistics +nan.0)
  (set-statistics-IQR! statistics +nan.0))

(define (statistics-tally! statistics value)
  (let ((n (+ (statistics-n statistics) 1))
        (data (statistics-data statistics)))
    ;; Increment number of data points.
    (set-statistics-n! statistics n)
    ;; Expand data vector if full.`
    (when (> n (vector-length data))
      (let ((new-data (make-vector (* 2 (vector-length data)))))
        (vector-copy! new-data 0 data)
        (set-statistics-data! statistics new-data)
        (set! data new-data)))
    ;; Insert new value.
    (let loop ((i (- n 1)))
      (if (and (> i 0)
               (<= value (vector-ref data (- i 1))))
          (begin
            (vector-set! data i (vector-ref data (- i 1)))
            (loop (- i 1)))
          (vector-set! data i value)))
    ;; Update sum.
    (set-statistics-sum! statistics (+ (statistics-sum statistics) value))
    ;; Update min and max.
    (set-statistics-xmin! statistics (min value (statistics-xmin statistics)))
    (set-statistics-xmax! statistics (max value (statistics-xmax statistics)))
    ;; Update range.
    (set-statistics-range! statistics (- (statistics-xmax statistics)
                                         (statistics-xmin statistics)))
    ;; Update quartiles and inter-quartile range (IQR).
    (let* ((n/4 (/ (- n 1) 4))
           (floor-n/4 (floor n/4))
           (ceiling-n/4 (ceiling n/4))
           (x1 (vector-ref data floor-n/4))
           (x2 (vector-ref data ceiling-n/4))
           (x (+ x1 (* (- n/4 floor-n/4) (- x2 x1)))))
      (set-statistics-x.25! statistics x))
    (let* ((n/2 (/ (- n 1) 2))
           (floor-n/2 (floor n/2))
           (ceiling-n/2 (ceiling n/2))
           (x1 (vector-ref data floor-n/2))
           (x2 (vector-ref data ceiling-n/2))
           (x (+ x1 (* (- n/2 floor-n/2) (- x2 x1)))))
      (set-statistics-x.5! statistics x))
    (let* ((3n/4 (/ (* 3 (- n 1)) 4))
           (floor-3n/4 (floor 3n/4))
           (ceiling-3n/4 (ceiling 3n/4))
           (x1 (vector-ref data floor-3n/4))
           (x2 (vector-ref data ceiling-3n/4))
           (x (+ x1 (* (- 3n/4 floor-3n/4) (- x2 x1)))))
      (set-statistics-x.75! statistics x))
    (set-statistics-IQR! statistics (- (statistics-x.75 statistics)
                                       (statistics-x.25 statistics)))
    ))

(provide (all-defined-out))
