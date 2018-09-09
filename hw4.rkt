#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; Problem #1
; Creates a list with values seperated by a stride from least to greatest
(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null))


; Problem #2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix )) xs)) 
  

; Problem #3
(define (list-nth-mod xs n)
  (cond [(negative? n) "list-nth-mod: negative number"]
        [(null? xs) "list-nth-mod: empty list"]
        [#t (remainder (car (list-tail xs n)) (length xs))]))


; Problem #4
(define (stream-for-n-steps s n)
  (cond [(= n 0) null]
        [(null? s) null]
        [#t (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))]))


; Problem #5
(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (modulo x 5) 0) (* -1 x) x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))


; Problem #6
(define dan-then-dog
  (letrec ([f (lambda (x) (cons (if (< (* -1 x) 0) "dan.jpg" "dog.jpg") (lambda () (f (* -1 x)))))])
    (lambda () (f 1))))


; Problem #7
(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons (cons 0 (car (x))) (lambda () (f (cdr (x))))))])
    (lambda () (f s))))
  

; Problem #8
(define (cycle-lists xs ys)
  (define sxs (make-stream xs))
  (define sys (make-stream ys))
(letrec ([f (lambda (x y k)
              (lambda () (cons (cons (car (stream-for-n-steps x k)) (car (stream-for-n-steps y k))) (f (cdr (x)) (cdr (y)) (+ k 1)))))])
  (f sxs sys 1)))
   

; Helper function for cycle-list, makes a stream from a list called ls
(define (make-stream ls)
         (letrec ([g (lambda (x) (cond[(null? x) ls] [#t x]))] ; Checks if list is empty, and resets list if so
                  [f (lambda (x) (cons (car (g x)) (lambda () (f (cdr (g x))))))])
           (lambda () (f ls))))


; Problem #9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n) (cond [(= (vector-length vec) n) #f] ; No match was found
                        [(pair? (vector-ref vec n))
                         (if (= v (car (vector-ref vec n)))
                             (vector-ref vec n)
                             (f (+ n 1)))]
                        [#t (f (+ n 1))]))])
    (f 0)))


; Problem # 10
(define (cached-assoc xs n)
   (letrec([memo (make-vector n #f)]
          [nxt 0] 
          [f (lambda (v)
               (let ([ans (vector-assoc v memo)])
                 (if ans
                     ans
                     (let ([new-ans (assoc v xs)])
                       (begin
                         (when new-ans
                           (vector-set! memo nxt new-ans)
                           (set! nxt (remainder (add1 nxt) n)))
                         new-ans)))))])
f))  