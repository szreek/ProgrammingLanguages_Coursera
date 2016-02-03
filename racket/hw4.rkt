#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

(define sequence
  (lambda (low high stride)
    (if (> low high) 
        null
        (cons low (sequence(+ low stride) high stride)))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [#t (letrec (
                   [rem (remainder n (length xs))]
                   [f (lambda (ys counter) (if (= counter rem) (car ys) (f (cdr ys)(+ counter 1))))])
              (f xs 0))]))

(define (list-nth-mod2 xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [#t (let (
                   [rem (remainder n (length xs))])
                   (if (= rem 0) (car xs) (car (list-tail xs rem))))]))
                   

(define (stream-for-n-steps s n)
  (if (zero? n) 
      null 
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1))))) 


(define funny-number-stream
  (letrec (
           [is-dive-five (lambda(y) (if (zero? (remainder y 5)) (- y) y))] 
           [f (lambda(x) (cons (is-dive-five x) (lambda()
                                    (f (+ x 1)))))])
    (lambda() (f 1))))


(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))] 
           [dog (lambda () (cons "dog.jpg" dan))])
    dan))
           

(define (stream-add-zero s)
  (letrec (
           [f (lambda (s2) (cons (cons 0 (car (s2))) (lambda () (f (cdr(s2))))))])
    (lambda () (f s))))


(define (cycle-lists xs ys)
  (letrec (
           [f (lambda (x-s y-s) (cond [(null? x-s) (f xs y-s)]
                                      [(null? y-s) (f x-s ys)]
                                      [#t (cons (cons (car x-s) (car y-s)) (lambda() (f (cdr x-s) (cdr y-s))))]
                                      ))])
    (lambda () (f xs ys))))


(define (vector-assoc v vec)
  (letrec (
           [l (vector-length vec)]
           [v? (lambda (pair)(if (equal? (car pair) v) #true #false))]
           [f (lambda (counter) 
                    (cond [(= l counter) #false] 
                          [(not (pair? (vector-ref vec counter))) (f (+ 1 counter))]
                          [(not (v? (vector-ref vec counter))) (f (+ 1 counter))]
                          [#true (vector-ref vec counter)]))])
    (f 0)))


(define (cached-assoc xs n)
  (letrec (
           [memo (make-vector n #f)]
           [slot 0])
    (lambda (v) 
      (let ([ans (vector-assoc v memo)])
        (if ans 
            ans
            (let ([ans2 (assoc v xs)])
              (if ans2
                  (begin (vector-set! memo slot ans2)
                         (set! slot (+ 1 slot))
                         ans2)
                  ans2)))))))