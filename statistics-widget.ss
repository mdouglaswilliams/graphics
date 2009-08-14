#lang scheme/gui

(require (planet williams/animated-canvas/animated-canvas)
         (planet williams/science/histogram)
         "quartile-statistics.ss")

(provide (all-defined-out))

(define statistics-widget%
  (class vertical-panel%
    ;; Init parameters
    (init-field label)
    (init-field n)
    (init-field min-range)
    (init-field max-range)
    (init parent)
    (init (font normal-control-font))
    ;; Instantiate superclass
    (super-instantiate (parent))
    ;; Create graphical subelements
    (define message
      (instantiate message%
        (label this)))
    (define canvas 
      (instantiate animated-canvas%
        (this)
        (style '(border))))
    ;; Recompute sizes and positions
    (send this reflow-container)
    ;; Create histogram vector
    (define histogram
      (make-histogram-with-ranges-uniform n min-range max-range))
    (define statistics
      (new-statistics))
    ;;
    (define/public (get-histogram)
      histogram)
    (define/public (get-statistics)
      statistics)
    ;;
    (define (map-x x)
      (let ((width (exact->inexact (send canvas get-width))))
        (* (/ (- x min-range) (- max-range min-range)) width)))
    ;; Draw histogram
    (define (draw-histogram (scale 1))
      (let* ((dc (send canvas get-dc))
             (width (exact->inexact (send canvas get-width)))
             (height (- (exact->inexact (send canvas get-height)) 50))
             (bin-width (/ width n))
             (bin-delta (/ (exact->inexact (- max-range min-range)) n))
             (half-bin-delta (/ bin-delta 2))
             (max-value (histogram-max histogram))
             (sum-value (histogram-sum histogram))
             (mean (histogram-mean histogram))
             (sigma (histogram-sigma histogram))
             (cum-value 0))
        (when (> sum-value 0.0)
          (send dc set-pen "White" 1 'solid)
          (for ((i (in-range n)))
            (let ((bin-center (+ min-range (* i bin-delta) half-bin-delta))
                  (bin-value (histogram-get histogram i)))
              (set! cum-value (+ cum-value bin-value))
              (unless (= bin-value 0)
                (let ((x1 (* i bin-width))
                      (bin-height (* scale (/  bin-value max-value) height))
                      (color
                       (cond ((<= bin-center (- mean (* 2.0 sigma))) "Red")
                             ((<= bin-center (- mean sigma)) "Yellow")
                             ((< bin-center (+ mean sigma)) "Green")
                             ((< bin-center (+ mean (* 2.0 sigma))) "Yellow")
                             (else "Red"))))
                  (send dc set-brush color 'solid)
                  (send dc draw-rectangle
                        x1 (- height bin-height)
                        bin-width bin-height)))))
          (send dc set-pen "Blue" 1 'solid)
          (let ((x (map-x mean)))
            (send dc draw-line x 0 x height))
          (send dc set-pen "Black" 1 'solid)
          (let ((x (map-x 0)))
            (send dc draw-line x 0 x (+ height 50)))
          (send dc draw-line 0 height width height)
          ;;
          (let-values (((inner-fence-lower inner-fence-upper)
                        (statistics-inner-fence statistics))
                       ((outer-fence-lower outer-fence-upper)
                        (statistics-outer-fence statistics)))
            (let ((y (+ height 25))
                  (mean (map-x (statistics-mean statistics)))
                  (x.25 (map-x (statistics-x.25 statistics)))
                  (x.5 (map-x (statistics-x.5 statistics)))
                  (x.75 (map-x (statistics-x.75 statistics)))
                  (fence-lower (map-x inner-fence-lower))
                  (fence-upper (map-x inner-fence-upper)))
              (send dc set-brush "black" 'transparent)
              (send dc draw-line fence-lower y x.25 y)
              (send dc draw-line x.75 y fence-upper y)
              (send dc draw-line fence-lower (- y 5) fence-lower (+ y 5))
              (send dc draw-line fence-upper (- y 5) fence-upper (+ y 5))
              (send dc draw-rectangle x.25 (- y 10) (- x.75 x.25) 21)
              (send dc draw-line x.5 (- y 10) x.5 (+ y 10))
              (send dc draw-line (- mean 5) y (+ mean 5) y)
              (send dc draw-line mean (- y 5) mean (+ y 5))
              )
            ;;
            (let ((y (+ height 25))
                  (data (statistics-data statistics)))
              (for ((i (in-range (statistics-n statistics))))
                (let ((point (vector-ref data i)))
                  (cond ((or (< point outer-fence-lower)
                             (> point outer-fence-upper))
                         (let ((x (map-x point)))
                           (send dc draw-line (- x 1) y (+ x 1) y)
                           (send dc draw-line x (- y 1) x (+ y 1))))
                        ((or (< point inner-fence-lower)
                             (> point inner-fence-upper))
                         (let ((x (map-x point)))
                           (send dc draw-ellipse (- x 1) (- y 1) 3 3)))))))
          ))
        (send canvas swap-bitmaps)))
    ;; Reset method
    (define/public (reset)
      (set! histogram
            (make-histogram-with-ranges-uniform n min-range max-range))
      (set! statistics (new-statistics))
      (draw-histogram))
    ;; Set value method
    (define/public (set-value value)
      (histogram-increment! histogram value)
      (statistics-tally! statistics value)
      (draw-histogram))))