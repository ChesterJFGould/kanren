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
        [(and
           (vector? a)
           (vector? b)
         )
         (if (= (vector-length a) (vector-length b))
           (unify-vectors 0 a b bindings)
           #f
         )
        ]
        [(and
           (record? a)
           (record? b)
         )
         (if (eq? (record-rtd a) (record-rtd b))
           (unify-records (record-rtd a) a b bindings)
           #f
         )
        ]
        [(equal? a b) bindings]
        [else #f]
      )
    )
  )

  (define (unify-vectors i a b bindings)
    (cond
      [(>= i (vector-length a)) bindings]
      [else
       (let
         [(bindings^ (unify (vector-ref a i) (vector-ref b i) bindings))]
         (if bindings^
           (unify-vectors (+ i 1) a b bindings^)
           #f
         )
       )
      ]
    )
  )

  (define (unify-records rtd a b bindings)
    (let
      [(bindings^
         (if (record-type-parent rtd)
           (unify-records (record-type-parent rtd) a b bindings)
           bindings
         )
       )
      ]
      (cond
        [bindings
          (unify-record-fields
            rtd
            (vector-length (record-type-field-names rtd))
            a
            b
            bindings
          )
        ]
        [else #f]
      )
    )
  )

  (define (unify-record-fields rtd num-fields a b bindings)
    (let recur
      [(i 0)
       (bindings bindings)
      ]
      (cond
        [(>= i num-fields) bindings]
        [else
         (let*
           [(accessor (record-accessor rtd i))
            (bindings^ (unify (accessor a) (accessor b) bindings))
           ]
           (cond
             [bindings^
               (recur (+ i 1) bindings^)
             ]
             [else #f]
           )
         )
        ]
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
