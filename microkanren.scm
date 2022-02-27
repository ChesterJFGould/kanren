(library (kanren microkanren)
  (export
    ==
    conj
    disj
    call/fresh
  )
  (import
    (chezscheme)
    (prefix
      (kanren bindings)
      bindings-
    )
    (prefix
      (kanren stream)
      stream-
    )
    (kanren var)
    (prefix
      (kanren state)
      state-
    )
    (prefix
      (kanren terms)
      terms-
    )
  )

  (define (== a b)
    (lambda (state)
      (let
        [(bindings^ (terms-unify a b (state-bindings state)))]
        (if bindings^
          (stream-one (state-update-bindings bindings^ state))
          stream-zero
        )
      )
    )
  )

  (define (call/fresh f)
    (lambda (state)
      (let-values
        [((var state^) (state-genvar state))]
        ((f var) state^)
      )
    )
  )

  (define (disj g1 g2)
    (lambda (state)
      (stream-add (g1 state) (g2 state))
    )
  )

  (define (conj g1 g2)
    (lambda (state)
      (stream-bind (g1 state) g2)
    )
  )

)
