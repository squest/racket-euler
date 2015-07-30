#lang racket

(define (sieve lim)
  (let* [(llim (integer-sqrt lim))
         (hlim (if (even? llim) (+ llim 1) (+ llim 2)))
         (primes (make-vector (+ lim 1) true))]
    (define (outer i res)
      (define (inner j)
        (when (<= j lim)
          (vector-set! primes j false)
          (inner (+ j i i))))
      (if (> i llim)
          res
          (if (vector-ref primes i)
              (begin (inner (* i i))
                     (outer (+ i 2) (cons i res)))
              (outer (+ i 2) res))))
    (append (reverse (outer 3 '(2)))
            (filter (λ (i) (vector-ref primes i)) (range hlim (+ lim 1) 2)))))

(define (sum-sieve lim)
  (let* [(llim (integer-sqrt lim))
         (hlim (if (even? llim) (+ llim 1) (+ llim 2)))
         (primes (make-vector (+ lim 1) true))]
    (define (outer i res)
      (define (inner j)
        (when (<= j lim)
          (vector-set! primes j false)
          (inner (+ j i i))))
      (if (> i llim)
          res
          (if (vector-ref primes i)
              (begin (inner (* i i))
                     (outer (+ i 2) (+ i res)))
              (outer (+ i 2) res))))
    (define (iter i res)
      (if (> i lim)
          res
          (if (vector-ref primes i)
              (iter (+ i 2) (+ i res))
              (iter (+ i 2) res))))
    (iter hlim (outer 3 2))))

(define-syntax-rule (fn bind body)
  (lambda bind body))





















