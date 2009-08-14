#lang scheme/gui

(require (planet williams/science/random-distributions)
         "quartile-statistics.ss"
         "statistics-widget.ss")

(define (main n)
  (send gauge set-value 0)
  (send gauge set-range n)
  (random-source-randomize! (current-random-source))
  (let loop ()
    (for ((i (in-range n)))
      (let ((x (if (<= (random) 0.05)
                   (random-flat 30.0 100.0)
                   (random-gaussian 30.0 6.0))))
        (send widget set-value x)
        (send gauge set-value i)))
    (if (eq? (message-box "Test Statistics Widget" "Press OK to loop or Cancel to quit."
                          #f '(ok-cancel))
             'ok)
        (begin
          (send widget reset)
          (loop))
        (send frame show #f))))

(define frame
  (instantiate frame% ("Test Statistics Widget")))

(define widget
  (instantiate statistics-widget%
    ("Perturbed Gaussian(30,6)" 40 0 100 frame)
    (min-width 600)
    (min-height 300)))

(define gauge
  (instantiate gauge%
    (#f 1 frame)))

(send frame show #t)

(main 2500)