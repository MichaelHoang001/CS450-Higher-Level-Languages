; 1: Is-list?
;
; This was a simple program until I tested it. It would EOF on non-list entities
; I soon figured out that (is-list? (cdr x)) would fail if x wasnt a pair
; such as x = (cons 4 5). When is-list reaches to the 5, it fails due to 5 not
; having a cdr.
(define (is-list? x)
  (cond ((null? x) #t)
        ((not (pair? x)) #f)
        ((is-list? (cdr x)) #t)
        (else #f)))
;(is-list? (list 1 2 3 4))
;(is-list? (cons 1 2))



; 2: Exercise 2.55
;
; (car '' abra) refers to the pair (' . abra) - where the cdr is abra and the
; car is the single quote. Likewise, (car 'abra) does not evaluate to quote
; because the abra is quoted out and is no longer part of a pair. Since this
; is no longer a pair, you cannot car it without hitting an error.



; 3: Exercise 2.18
;
; This was was a bastardized version of the (append x y) that we covered in
; Lecture 3. I just flipped the actual appending, and had it append to an
; empty list. I tested it once and to my surprise, it worked perfectly on
; any list I tried - with the caveat that it seems to lose it's 'listness'.
; It only outputs in a nested pair, such as ((((() 4 5) 3) 2) 1).
; I managed to fix this by reworking everything to be iterative and not
; recursive.
(define (my-reverse x)
  (define (revItr x y)
    (cond ((null? x) y)
          (else (revItr (cdr x) (cons (car x) y)))))
  (if (pair? x)
      (revItr x '())
      x)) ; In case you are reversing a non-list, wont crash if reversing a num  
;(my-reverse '(1 2 3 (4 5)))



; 4: Exercise 2.20
;
; My first instinct was to split the function into two parts - if the first
; entry was even or odd, I would split them into parityeven or parity odd, which
; would be the same code but check for evenness or oddness. I then realized I
; could just write the code once, but check it against (modulo first 2).
; I ran into a snag where I couldn't get the list to be printed in the correct
; order. Something weird with the cons, I'm sure. Instead, I cut corners since
; I knew the answer was correct, and I had already programmed a response to
; needing a list to be reversed! My apologies.
(define (same-parity first . x)
  (define (parityItr first input output)
    (cond ((null? input) output)
          ((= (modulo (car input) 2) (modulo first 2))
               (parityItr first (cdr input) (cons (car input) output)))
          (else (parityItr first (cdr input) output))))
  (my-reverse (parityItr first x (list first))))

;(same-parity 1 2 3 4 5 6 7 8 9 10)
;(same-parity 2 4 6 8 10 1 1 1 1 1)



; 5: Exercise 2.21
;
; Simple question. First one was copied almost word for word from the
; (map func sequence) function in lecture 3. The second was my best guess at how
; the map function works. I'm not sure if there is a way to use map without
; quickly defining a helper function immediately before it.
(define (square-list1 items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list1 (cdr items)))))

(define (square-list items)
  (define (sq x)
    (* x x))
  (map sq items))
;(square-list (list 1 2 3 4 5))



; 6: Exercise 2.23
;
; I actually was confused about what this exercise wanted. I thought I was to
; write a for-each loop that displayed elements, and so I set about hardcoding
; that. Piazza saved the day when a student's post made me realize that the
; business with the lambda was the test case, not the function! Then, it was
; trivial to simplify problem 5 into something that could work for everything.
; I really appreciate the clear line of thought that these problems lay out,
; I can clearly see the lessons I am to learn, and can immediately put them to
; use.
(define (my-for-each func x)
  (map func x))
;(my-for-each (lambda (x) (newline) (display x)) (list 1 2 3 4 5))



; 8: Exercise 2.27
;
; Very confusing. I've had to rework this one multiple times, and ended up
; having to edit my own reverse function in order to keep it all smooth.
; I realize that this only deep-reverses two steps in -
; Something like ( ((1 2) (3 4)) 5) wouldn't be correct. But I don't know
; what the proper base case for the recursion would be, especially since I am
; using my own function. Headaches abound.
(define (my-deep-reverse x)
  (map my-reverse (my-reverse x)))
;(my-deep-reverse '(1 2 3 (4 5)))



; 9: Exercise 2.54
;
; This is the first program that I thought that correct answer would take up
; a lot of space, so I condensed much of it into helper functions. Turns out
; this was a huge boon towards turning all this mumbo jumbo into readable text.
; It's almost english at this point! Once I hit that point, it was just a matter
; of figuring out the checks that such a function would do. I was overjoyed when
; it worked flawlessly the first time - any programmer's dream!
(define (my-equal? a b)
  (define (both check? a b) ; these 3 just to save a few keystrokes
    (and (check? a) (check? b)))
  (define (eq-car a b)
    (my-equal? (car a) (car b)))
  (define (eq-cdr a b)
    (my-equal? (cdr a) (cdr b)))

  (cond ((both null? a b) #t)
        ((both pair? a b) (and (eq-car a b) (eq-cdr a b)))
        ((both integer? a b) (= a b))
        (else #f)))
;(my-equal? '(1 2 3 (4 5) (6 (7 8))) '(1 2 3 (4 5) (6 (7 8))))
;(my-equal? '(1 2 3 (4 5) (6 (7 8))) '(1 2 3 (4 5) (6 9)))



; 10 every?
;
; I.. struggled to answer the main question. If lists are empty, do they fail
; all predicates due to not having any trues, or do they pass all predicates
; due to not having any falses? I don't remember what I was taught in Discrete.
; I'm going to have to base my answer - more of a guess, really - on common
; sense. I guess It'd have to return false, due to an empty list not serving
; whatever purpose you're looking for? I wrote this section before I starting
; coding, so I guess I'll find out soon enough.
(define (every? pred? seq)
  (cond ((null? seq) #t)
        ((pair? seq) (and (every? pred? (car seq)) (every? pred? (cdr seq))))
        ((integer? seq) (pred? seq))
        (else #f)))

;(every? (lambda (x) (> x 0)) '(1 2 (3 (4 5 6 (7 8))) 9))
;(every? (lambda (x) (> x 0)) '(1 2 (3 (4 5 6 (7 0))) 9))
;(every? (lambda (x) (> x 0)) '())



; 11 Exercise 2.59
;
; I had to rip the element-of-set? from the book, and was worried that it might
; not work due to minor changes or misuse.
(define (unordered-union-set a b)
  (define (element? x set)
    (cond ((null? set) #f)
          ((equal? x (car set)) #t)
          (else (element? x (cdr set)))))
  (define (UUS-itr a b ans)
    (cond ((null? a) ans)
          ((element? (car a) b) (UUS-itr (cdr a) b (cons (car a) ans)))
          (else (UUS-itr (cdr a) b ans))))
  (reverse (UUS-itr a b '())))
;(unordered-union-set '(1 2 3) '(2 3 4))



; 12 Exercise 2.62
;
; Now comes the time efficiency struggles! I can't use element-of-set due to it
; being O(n) and parsing through the first set makes that necessarily O(n^2).
(define (ordered-union-set a b)
  (define (OUS-itr a b ans)
    (cond ((or (null? a) (null? b)) ans)
          ((= (car a) (car b)) (OUS-itr (cdr a) (cdr b) (cons (car a) ans)))
          ((> (car a) (car b)) (OUS-itr a (cdr b) ans))
          ((< (car a) (car b)) (OUS-itr (cdr a) b ans))))
  (reverse (OUS-itr a b '())))
;(ordered-union-set '(1 2 3 10 11 12) '(2 7 8 10 11)) 



; 13 remove-val
;
; Not really editing the list, just making a new one but selectively.
; Probably not the intended way to do this, but it works!
(define (remove-val v x)
  (define (remval-itr v x ans)
    (cond ((null? x) ans)
          ((not (= v (car x))) (remval-itr v (cdr x) (cons (car x) ans)))
          (else (remval-itr v (cdr x) ans))))
  (reverse (remval-itr v x '())))
(remove-val 5 '(1 2 3 4 5 6 7 6 5 4 5 5))
