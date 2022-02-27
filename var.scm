(library (kanren var)
  (export
    var?
    make-var
    var=?
    var->symbol
    next-var
  )
  (import
    (rnrs)
  )

  (define-record-type var (fields var))

  (define (var=? a b)
    (= (var-var a) (var-var b))
  )

  (define (var->symbol var)
    (string->symbol (string-append "_." (number->string (var-var var))))
  )

  (define (next-var var)
    (make-var (+ (var-var var) 1))
  )
)
