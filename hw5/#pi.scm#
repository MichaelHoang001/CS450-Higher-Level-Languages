;; cons-stream is already defined (by a macro, as a special form) in
;; UMB Scheme

(define stream-car car)
(define (stream-cdr stream) (force (cdr stream)))
(define stream-null? null?)
(define the-empty-stream '())

(define (stream-foreach f x)
  (if (stream-null? x)
      'done
      (begin (f (stream-car x))
             (stream-foreach f (stream-cdr x)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

;;; =============== DELETE THESE BEFORE SUBMITTING ===============
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (out x . y)
  (newline)
  (display x)
  (map (lambda (q) (display " ") (display q)) y))
;;; =============== DELETE THESE BEFORE SUBMITTING ===============

;; problem 1

(define (display-n stream n)
  (cond ((and (not (stream-null? stream)) (> n 0))
         (newline)
         (display (stream-car stream))
         (display-n (stream-cdr stream) (- n 1))
         )))

;; problem 2
;; 3.50 p 440

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;; problem 3
;;(define (add-streams s1 s2)
;;  (stream-map + s1 s2))

(define (notdiv235? x)
  (not (or (= (remainder x 2) 0)
           (= (remainder x 3) 0)
           (= (remainder x 5) 0))))
(define notdiv-235 (stream-filter notdiv235? integers))

;; problem 4
(define (num->list in) ;integer
 (map (lambda (x) (- (char->integer x) 48))
      (string->list (number->string in))))
(define (leftpad list len) ;input list, expected length
  (if (>= (length list) len)
      list
      (leftpad (cons 0 list) len)))
(define (parsepow a-list)
  (define (iter i pow)
    (cond ((= 0 i) pow)
          ((= 0 pow) (iter (- i 1) 1))
          (else (iter (- i 1) (* 10 pow)))))
      (iter (length a-list) 0))

(define (mult-stream m` strm`) ;factor 1 (integer), factor 2 (stream)
  (define (action m a a-list pow strm)
    (cond ((stream-null? strm) a-list)
          (else (if (and (not (null? a-list)) (< (+ m (modulo a pow)) pow))
                    (produce m a a-list pow strm)
                    (consume m a a-list pow strm)))))
  (define (produce m a a-list pow strm)
    (cons-stream
     (car a-list)
     (action m (modulo a pow) (cdr a-list) (parsepow (cdr a-list)) strm)))
  (define (consume m a a-list pow strm)
    (let ((a.new (+ (* 10 a) (* m (stream-car strm)))))
      (let ((a-list.new (leftpad (num->list a.new) (length a-list))))
        (action m a.new a-list.new (parsepow a-list.new) (stream-cdr strm)))))
  (action m` 0 '() 0 strm`))

;; problem 5


  
