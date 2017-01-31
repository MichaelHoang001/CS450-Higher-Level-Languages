;; Program 1
(define make-account-lambda
  (lambda (balance)
    (define withdraw (lambda (amount)
                       (if (>= balance amount)
                           (begin (set! balance (- balance amount))
                                  balance)
                           "Insufficient funds")))
    (define deposit (lambda (amount)
                      (set! balance (+ balance amount))
                      balance))
    (lambda (m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))))

(define make-account-inline
  (lambda (balance)
    (lambda (m)
      (cond ((eq? m 'withdraw)
             (lambda (amount)
               (if (>= balance amount)
                   (begin (set! balance (- balance amount)) 
                          balance)
                   "Insufficient funds")))
            ((eq? m 'deposit) 
             (lambda (amount)
               (set! balance (+ balance amount))
               balance))
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))))

;(define acc (make-account-lambda 50))
;((acc 'deposit) 40)
;((acc 'withdraw) 60)
;(define acc2 (make-account-inline 50))
;((acc2 'deposit) 40)
;((acc2 'withdraw) 60)

;; Program 3
;exercise 3.2, page 304
(define make-monitored
  (let ((count 0))
    (lambda (f)
      (lambda (input)
        (cond ((eq? input 'how-many-calls?)
               count)
              ((eq? input 'reset-count)
               (set! count 0))
              (else (set! count (+ count 1))
                    (f input)))))))


;; Program 4
;In doing this problem, build on your solution to Problem 1. In fact, see if you
;can use one of your solutions to Problem 1 as a "black box" -- that is, make 
;the solution to this problem a "wrapper" procedure that just invokes one of the 
;versions of make-account from Problem 1, after handling password checks. In
; this way, you don't have to copy any of the body of the original make-account
; procedure.

;Call your new function make-pw-account. And yes, I really meant this - note
; that this is different from what the book says.

(define make-pw-accountWORK
  (let ((pw-saved 'default))
    (lambda (balance pw-in)
      (set! pw-saved pw-in)
      (define withdraw (lambda (amount)
                         (if (>= balance amount)
                             (begin (set! balance (- balance amount))
                                    balance)
                             "Insufficient funds")))
      (define deposit (lambda (amount)
                        (set! balance (+ balance amount))
                        balance))
      (lambda (pw-in m)
        (cond ((eq? pw-saved pw-in)
               (cond ((eq? m 'withdraw) withdraw)
                     ((eq? m 'deposit) deposit)
                     (else (error "Unknown request: MAKE-ACCOUNT"
                                  m))))
              (else (error "Incorrect password")))))))

;(define acc (make-pw-account 50 'twenty))
;((acc 'twenty 'withdraw) 40)
;((acc 'bad 'deposit) 50)
