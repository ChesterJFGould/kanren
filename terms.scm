(library (kanren terms)
  (export
    walk*
    unify
    var-map
  )
  (import
    (rnrs)
    (kanren var)
    (prefix
      (kanren bindings)
      bindings-
    )
  )

  (define (walk term bindings)
    (cond
      [(not (var? term)) term]
      [else
        (let [(next-term (bindings-lookup term bindings))]
          (if next-term
            (walk next-term bindings)
            term
          )
        )
      ]
    )
  )

  (define (walk* term bindings)
    (let
      [(term^ (walk term bindings))]
      (cond
        [(var? term^) term^]
        [(pair? term^)
         (cons
           (walk* (car term^) bindings)
           (walk* (cdr term^) bindings)
         )
        ]
        [else term^]
      )
    )
  )

  (define (unify a b bindings)
    (let
      [(a (walk a bindings))
       (b (walk b bindings))
      ]
      (cond
        [(and
           (var? a)
           (var? b)
           (var=? a b)
         )
         bindings
        ]
        [(var? a)
         (bindings-insert a b bindings)
        ]
        [(var? b)
         (bindings-insert b a bindings)
        ]
        [(and
           (pair? a)
           (pair? b)
         )
         (let
           [(bindings^ (unify (car a) (car b) bindings))]
           (if bindings^
             (unify (cdr a) (cdr b) bindings^)
             #f
           )
         )
        ]
        [(equal? a b) bindings]
        [else #f]
      )
    )
  )

  (define (var-map f term)
    (cond
      [(var? term) (f term)]
      [(pair? term)
       (cons
         (var-map f (car term))
         (var-map f (cdr term))
       )
      ]
      [else term]
    )
  )
)
