(library (kanren bindings)
  (export
    empty
    insert
    lookup
  )
  (import
    (rnrs)
    (kanren var)
  )

  (define empty '())

  (define (insert var term map)
    (cons (cons var term) map)
  )

  (define (lookup var map)
    (cond
      [(null? map) #f]
      [(var=? (caar map) var) (cdar map)]
      [else (lookup var (cdr map))]
    )
  )
)
