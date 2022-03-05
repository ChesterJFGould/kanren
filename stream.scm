(library (kanren stream)
  (export
    zero
    zero?
    one
    add
    xor
    bind
    take-all
    take-n
  )
  (import
    (except (rnrs) zero?)
  )

  (define zero '())

  (define zero? null?)

  (define (one a) (cons a zero))

  (define (add a b)
    (cond
      [(zero? a) b]
      [(procedure? a) (lambda () (add b (a)))]
      [else (cons (car a) (add b (cdr a)))]
    )
  )

  (define (xor a b)
    (cond
      [(zero? a) b]
      [(procedure? a) (lambda () (xor b (a)))]
      [(and
         (pair? a)
         (pair? b)
       )
      ]
      [else (cons (car a) (add b (cdr a)))]
    )
  )

  (define (bind s f)
    (cond
      [(zero? s) zero]
      [(procedure? s) (lambda () (bind (s) f))]
      [else (add (f (car s)) (bind (cdr s) f))]
    )
  )

  (define (advance s)
    (cond
      [(procedure? s) (advance (s))]
      [else s]
    )
  )

  (define (take-all s)
    (cond
      [(zero? s) s]
      [(procedure? s) (take-all (advance s))]
      [else (cons (car s) (take-all (cdr s)))]
    )
  )

  (define (take-n n s)
    (cond
      [(= 0 n) zero]
      [(zero? s) zero]
      [(procedure? s) (take-n n (advance s))]
      [else (cons (car s) (take-n (- n 1) (cdr s)))]
    )
  )
)
