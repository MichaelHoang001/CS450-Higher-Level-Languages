;; read-file produces a list whose elements are the expressions in the file.
(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))
(define source (with-input-from-file "units.dat" read-file))

;; the actual conversion func
(define (convert aQuan bUnitlist)
  (let ((aNorm (parse-quantity (car aQuan) (cdr aQuan)))
        (bNorm (parse-quantity 1 bUnitlist)))
(display aNorm)
(display bNorm)
    (cond ((not (compatible? aNorm bNorm)) #f)
          (else (cons (/ (car aNorm) (car bNorm)) bUnitlist)))))

;; helper function to look up a unit from source
(define (search-source req) ;; called with requested unit's name
  (define (searchRecur node req)
    (cond ((null? node) null)
          ((equal? req (caar node)) (car node))
          (else (searchRecur (cdr node) req))))
  (searchRecur source req))

;; normalize a unitlist into a baselist of 4 numbers: quantity, kg, m, sec
(define (normalize in exp prior) ;; called with unitlist, its exponent
  (define (nrmBase u)            ;; and any prior list 
    (nrmRecur u (cdadr in)))
  (define (nrmRecur u list)
    (cond ((null? list) 0)
          ((equal? u (caar list)) (* exp (cadar list)))
          (else (nrmRecur u (cdr list)))))

  (define nrmQuan
    (cond ((> exp 0) (* (car prior) (caadr in)))
          (else (/ (car prior) (caadr in)))))
  (list nrmQuan                             ;; make a new list that is the
         (+ (cadr prior)   (nrmBase 'kg))   ;; merging of this unitlist and
         (+ (caddr prior)  (nrmBase 'm))    ;; any prior result. default to
         (+ (cadddr prior) (nrmBase 'sec))));; a prior list of '(1 0 0 0).

;; condense an entire custom quantity into a baselist
(define (parse-quantity quantity list-of-units)
  (define (parseRecur baseL unitL) ;; condensed baselist tally, list-of-units
    (cond ((null? unitL) baseL)
          (else (parseRecur
                 (normalize (search-source (caar unitL)) (cadar unitL) baseL)
                 (cdr unitL)))))
  (parseRecur (list quantity 0 0 0) list-of-units))

;; checks if two normalized quantities have the same base units
(define (compatible? a b)
  (and (= (cadr a) (cadr b))      ;kg
       (= (caddr a) (caddr b))    ;m
       (= (cadddr a) (cadddr b))));sec

                                        ;TESTING AREA
;(convert '(27.5 (furlong 1)(fortnight -1)) '((mi 1)(hr -1)))
;(convert '(42.78 (joule 1)(mph -1)) '((mg 1)(furlong 1)(fortnight -1)))
