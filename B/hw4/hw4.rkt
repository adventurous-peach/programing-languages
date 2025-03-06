
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file
(require rackunit)

;; put your code below

(define (sequence l h s)
  (if (> l h) '() (cons l (sequence (+ l s) h s))))

(define (string-append-map l suffix)
  (map (lambda (e) (string-append e suffix)) l))

(define (list-nth-mod l n)
  (cond
    [(negative? n) (error "list-nth-mod: negative number")]
    [(empty? l) (error "list-nth-mod: empty list")]
    [else (car (list-tail l (remainder n (length l))))]))

(define (stream-for-n-steps s n)
  (let ([s (s)])
        (cond
          [(zero? n) '()]
          [else (cons (car s) (stream-for-n-steps (cdr s) (- n 1)))])))

(define (funny-number-stream)
  (letrec ([h (lambda (n) (cons (if (zero? (remainder n 5)) (- n) n) (lambda() (h (add1 n)))))])
    (h 1)))

(define (dan-then-dog)
  (letrec ([helper-fun (lambda (n) (if (odd? n)
                                       (cons "dan.jpg" (lambda() (helper-fun 0)))
                                       (cons "dog.jpg" (lambda () (helper-fun 1)))))])
    (helper-fun 1)))

(define (stream-add-zero stream)
  (letrec ([helper-fun (lambda (s) (cons (cons 0 (car (s))) (stream-add-zero (cdr (s)))))])
    (lambda () (helper-fun stream))))

(define (cycle-lists l1 l2)
  (letrec ([helper-fun (lambda(n) (cons (cons (list-nth-mod l1 n) (list-nth-mod l2 n))
                                        (lambda () (helper-fun (add1 n)))))])
    (lambda() (helper-fun 0))))


(define (vector-assoc v vec)
  (letrec ([vec-length (vector-length vec)]
           [traverse-vector (lambda (i) (if (= i vec-length) #f
                                            (let ([e (vector-ref vec i)])
                                              (cond
                                                 [(pair? e) (if (equal? v (car e)) e
                                                                (traverse-vector (add1 i)))]
                                                 [else (traverse-vector (add1 i))]))))])
    (traverse-vector 0)))

(define (cached-assoc l n)
  (letrec ([l-length (length l)]
           [cache (make-vector n #f)]
           [i 0]
           [look-up-and-update-cache (lambda (v) (let ([ans (assoc v l)])
                                                   (if ans (begin
                                                             (vector-set! cache i ans)
                                                             (if (= (add1 i) l-length) 0 (set! i (add1 i)))
                                                             ans)
                                                       #f)))])
    (lambda (v) (let ([cached-ans (vector-assoc v cache)])
                  (if cached-ans cached-ans (look-up-and-update-cache v))))))
