#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride) 
  (if (> low high) null (if (> (+ low stride) high) (cons low null) (cons low (sequence (+ low stride) high stride)))))

(define (string-append-map xs suffix)
  (map (lambda (a) (string-append a suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0)    (error "list-nth-mod: negative number")] 
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t      (car (list-tail xs (remainder n (length xs))))]))

(define ones
  (lambda () (cons 1 ones)))

(define (stream-for-n-steps s n)
  (if (= n 0) null (let ([pair (s)])
                     (cons (car pair) (stream-for-n-steps (cdr pair) (- n 1))))))

(define funny-number-stream 
  (letrec ([f (lambda (n) (let ([maybe-neg-n (if (= 0 (modulo n 5)) (* n -1) n)]) 
                            (cons maybe-neg-n (lambda () (f(+ (abs n) 1))))))]) 
    (lambda () (f 1))))

(define dan-then-dog 
  (letrec ([dan-fun (lambda () (cons "dan.jpg" dog-fun))]
        [dog-fun (lambda () (cons "dog.jpg" dan-fun))])
   (lambda () (dan-fun))))

(define (stream-add-zero s) (lambda () (let ([pair (s)])
                    (cons (cons 0 (car pair)) (stream-add-zero (cdr pair))))))


(define (cycle-lists xs ys) 
  (define (list-to-stream lst) (lambda () (letrec ([f (lambda (ls) (let* ([head (car ls)]
                                                  [tail (cdr ls)]
                                                  [new-list (append tail (cons head null))])
                                              (cons head (lambda () (f new-list)))))])
                              (f lst))))
  (define (merge-streams stream-1 stream-2) (lambda () (let ([s-pair-1 (stream-1)]
                                         [s-pair-2 (stream-2)])
                                      (cons (cons (car s-pair-1) (car s-pair-2)) (merge-streams (cdr s-pair-1) (cdr s-pair-2))))))
  (merge-streams (list-to-stream xs) (list-to-stream ys)))

(define (vector-assoc v vec)
  (letrec ([filtered-vector (vector-filter pair? vec)]
         [match (lambda (vec) (cond [(equal? '#() vec) #f]
                          [(equal? v (car (vector-ref vec 0))) (vector-ref vec 0)]
                          [(equal? (vector-length vec) 1) #f]
                          [#t (match (vector-copy vec 1 (vector-length vec)))]))])
   (match filtered-vector))) 

(define (cached-assoc xs n) 
  (letrec ([cache (make-vector n #f)]
           [open-slot 0]
           [f (lambda (v) 
                (let ([cache-pair (vector-assoc v cache)])
                  (if (pair? cache-pair) cache-pair 
                    (let ([list-pair (assoc v xs)])
                      (if (pair? list-pair) 
               (begin (vector-set! cache open-slot list-pair) 
                    (if (= open-slot (- (vector-length cache) 1))
                     (set! open-slot 0)
                     (set! open-slot (+ open-slot 1))) 
                    list-pair) 
               #f)))))])
   f))