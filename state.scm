(library (kanren state)
  (export
    empty
    update-bindings
    bindings
    genvar
    reify/1st-var
  )
  (import
    (rnrs)
    (kanren var)
    (prefix
      (kanren bindings)
      bindings-
    )
    (prefix
      (kanren terms)
      terms-
    )
  )

  (define-record-type state (fields (immutable bindings bindings) next-var))

  (define empty (make-state bindings-empty (make-var 0)))

  (define (update-bindings bindings state)
    (make-state bindings (state-next-var state))
  )

  (define (update-next-var var state)
    (make-state (bindings state) var)
  )

  (define (genvar state)
    (values
      (state-next-var state)
      (update-next-var (next-var (state-next-var state)) state)
    )
  )

  (define (reify/1st-var state)
    (let
      [(term (terms-walk* (make-var 0) (bindings state)))]
      (terms-var-map var->symbol term)
    )
  )
)
