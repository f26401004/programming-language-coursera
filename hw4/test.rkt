
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence start end gap)
  (if (> start end)
    (list)
    (cons start (sequence (+ start gap) end gap))))
    

(define (string-append-map xs suffix) 
  (map (lambda (string) (string-append xs suffix)) xs))


(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (<= n 0)
    null
    (cons (car (s))
          (stream-for-n-steps (cdr (s)) (- n 1)))))

(define (funny-number-stream)
  (define (f x) (cons (if (= (remainder x 5) 0) (- x) x)
                      (lambda () (f (+ x 1)))))
  (f 1))

(define (dan-then-dog)
  (define (f x) (cons (if (= (remainder x 2) 0) "dan.jpg" "dog.jpg")
                      (lambda () (f (+ x 1)))))
  (f 0))

(define (stream-add-zero s)
  (define (f x) (
    cons 
      (cons 0 (car (s)))
      (lambda () (f (cdr (x))))))
  (f s))

(define (cycle-lists xs ys)
  (define (f x) 
    (cons 
      (cons
        (list-nth-mod xs x)
        (list-nth-mod ys x))
      (lambda () (f (+ x 1)))))
  (f 0))

(define (vector-assoc v vec)
  (define (f x) (
      (cond [(= (vector-length vec) 0) #f]
          [(>= x (vector-length vec)) #f]
          [(pair? (vector-ref vec x)) (f (+ x 1))]
          [(equal? (vector-ref vec x) v) (vector-ref vec x)]
          [else (f (+ x 1))])))
  (f 0)
)

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
          [next 0]
          [f (lambda (v)
            (let ([ans (vector-assoc v memo)])
              (if ans
                ans
                (let ([new-ans (assoc v xs)])
                  (begin
                    (when new-ans
                      (vector-set! memo next new-ans)
                      (set! next (remainder (+ next 1) n)))
                      new-ans)))))])
  f))
