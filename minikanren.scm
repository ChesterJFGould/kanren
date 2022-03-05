(library (kanren minikanren)
  (export
    conde
    condx
    fresh
    run
    run*
    ==
  )
  (import
    (rnrs)
    (kanren microkanren)
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
      (kanren bindings)
      bindings-
    )
  )

  (define-syntax Zzz
    (syntax-rules ()
      ((_ g) (lambda (state) (lambda () (g state))))
    )
  )

  (define-syntax conj+
    (syntax-rules ()
      ((_ g) (Zzz g))
      ((_ g g* ...) (conj (Zzz g) (conj+ g* ...)))
    )
  )

  (define-syntax disj+
    (syntax-rules ()
      ((_ g) (Zzz g))
      ((_ g g* ...) (disj (Zzz g) (disj+ g* ...)))
    )
  )

  (define-syntax excl+
    (syntax-rules ()
      ((_ g) (Zzz g))
      ((_ g g* ...) (excl (Zzz g) (excl+ g* ...)))
    )
  )

  (define-syntax conde
    (syntax-rules ()
      ((_ (g g* ...) ...) (disj+ (conj+ g g* ...) ...))
    )
  )

  (define-syntax condx
    (syntax-rules ()
      ((_ (g g* ...) ...) (excl+ (conj+ g g* ...) ...))
    )
  )

  (define-syntax fresh
    (syntax-rules ()
      ((_ () g g* ...) (conj+ g g* ...))
      ((_ (x x* ...) g g* ...)
        (call/fresh (lambda (x) (fresh (x* ...) g g* ...)))
      )
    )
  )

  (define-syntax run
    (syntax-rules ()
      ((_ n (x x* ...) g g* ...)
        (map state-reify/1st-var
          (stream-take-n n ((fresh (x x* ...) g g* ...) state-empty))
        )
      )
    )
  )

  (define-syntax run*
    (syntax-rules ()
      ((_ (x x* ...) g g* ...)
        (map state-reify/1st-var
          (stream-take-all ((fresh (x x* ...) g g* ...) state-empty))
        )
      )
    )
  )
)
