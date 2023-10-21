(module types (lib "eopl.ss" "eopl")

  (provide type?
           int-type?
           bool-type?
           proc-type?
           type-variable?
           int-type
           bool-type
           proc-type
           fresh-type-variable
           proc-type-argument-types
           proc-type-result-type
           make-type-constraint
           type-constraint-lhs
           type-constraint-rhs
           solve-type-constraints)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Abstract data types for Type and TypeConstraint
  ;;
  ;; type?          : Any -> Boolean
  ;; int-type?      : Any -> Boolean
  ;; bool-type?     : Any -> Boolean
  ;; proc-type?     : Any -> Boolean
  ;; type-variable? : Any -> Boolean
  ;; int-type            : -> Type
  ;; bool-type           : -> Type
  ;; proc-type           : Listof[Type] * Type -> Type
  ;; fresh-type-variable : -> Type
  ;; fresh-type-variable : Exp -> Type          (overloaded)
  ;; proc-type-argument-types : Type -> Listof[Type]
  ;; proc-type-result-type    : Type -> Type
  ;;
  ;; make-type-constraint : Type * Type -> TypeConstraint
  ;; type-constraint-lhs  : TypeConstraint -> Type
  ;; type-constraint-rhs  : TypeConstraint -> Type
  ;; solve-type-constraints : Listof[TypeConstraint] -> Boolean
  ;;
  ;; Algebraic specification:
  ;;
  ;; (type? (int-type)) = #t
  ;; (type? (bool-type)) = #t
  ;; (type? (proc-type argtypes resulttype)) = #t
  ;; (type? (fresh-type-variable)) = #t
  ;;
  ;; (int-type? (int-type)) = #t
  ;; (int-type? (bool-type)) = #f
  ;; (int-type? (proc-type argtypes resulttype)) = #f
  ;; (int-type? (fresh-type-variable)) = #f
  ;;
  ;; (bool-type? (int-type)) = #f
  ;; (bool-type? (bool-type)) = #t
  ;; (bool-type? (proc-type argtypes resulttype)) = #f
  ;; (bool-type? (fresh-type-variable)) = #f
  ;;
  ;; (proc-type? (int-type)) = #f
  ;; (proc-type? (bool-type)) = #f
  ;; (proc-type? (proc-type argtypes resulttype)) = #t
  ;; (proc-type? (fresh-type-variable)) = #f
  ;;
  ;; (type-variable? (int-type)) = #f
  ;; (type-variable? (bool-type)) = #f
  ;; (type-variable? (proc-type argtypes resulttype)) = #f
  ;; (type-variable? (fresh-type-variable)) = #t
  ;;
  ;; (proc-type-argument-types argtypes resulttype) = argtypes
  ;; (proc-type-result-type argtypes resulttype) = resulttype
  ;;
  ;; (type-constraint-lhs (make-type-constraint ty1 ty2)) = ty1
  ;; (type-constraint-rhs (make-type-constraint ty1 ty2)) = ty2
  ;;
  ;; If the constraints are satisfiable, then
  ;;     (solve-type-constraints constraints) = #t
  ;; If the constraints are not satisfiable, then
  ;;     (solve-type-constraints constraints) = #f
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;; type? : Any -> Boolean

  (define (type? x)
    (or (int-type? x)
        (bool-type? x)
        (proc-type? x)
        (type-variable? x)))

  ;; Types are an abstract data type, so no client code
  ;; should depend upon their representation, which is
  ;; subject to change without notice.
  ;;
  ;; For now, types are represented by lists
  ;; as specified by the following grammar.
  ;;
  ;; <type>  ::=  (int)
  ;;         ::=  (bool)
  ;;         ::=  (proc (<type> ...) <type>)
  ;;         ::=  (tvar <integer>)

  ;; int-type? : Any -> Boolean

  (define (int-type? x)
    (equal? x (int-type)))

  ;; bool-type? : Any -> Boolean

  (define (bool-type? x)
    (equal? x (bool-type)))

  ;; proc-type? : Any -> Boolean

  (define (proc-type? x)
    (and (eq? (car x) 'proc)
         (= 3 (length x))
         ((list-of type?) (cadr x))
         (type? (caddr x))))

  ;; type-variable? : Any -> Boolean

  (define (type-variable? x)
    (and (pair? x)
         (pair? (cdr x))
         (null? (cddr x))
         (eq? (car x) 'tvar)
         (integer? (cadr x))))

  ;; int-type : -> Type

  (define (int-type) '(int))

  ;; bool-type : -> Type

  (define (bool-type) '(bool))

  ;; proc-type : Listof[Type] * Type -> Type

  (define (proc-type argument-types result-type)
    (if (not (and ((list-of type?) argument-types)
                  (type? result-type)))
        (eopl:error 'proc-type "Illegal arguments"))
    (list 'proc argument-types result-type))

  ;; fresh-type-variable : -> Type
  ;; fresh-type-variable : Exp -> Type
  ;;
  ;; (fresh-type-variable) returns a new type variable.
  ;; (fresh-type-variable exp) returns a new type variable
  ;;     but also records an association between that new
  ;;     type variable and exp, which is used to generate
  ;;     error messages.

  (define (fresh-type-variable . rest)
    (set! type-variable-counter (+ type-variable-counter 1))
    (let ((tvar (list 'tvar type-variable-counter)))
      (if (not (null? rest))
          (set! type-variable-expressions
                (cons (list tvar exp)
                      type-variable-expressions)))
      tvar))

  ;; proc-type-argument-types : Type -> Listof[Type]
  ;;
  ;; If ty is a proc type, then
  ;; (proc-type-argument-types ty) returns the list of argument types.

  (define (proc-type-argument-types ty)
    (if (proc-type? ty)
        (cadr ty)
        (eopl:error 'proc-type-argument-types "Invalid proc type.")))

  ;; proc-type-result-type : Type -> Listof[Type]
  ;;
  ;; If ty is a proc type, then
  ;; (proc-type-result-types ty) returns the result type.

  (define (proc-type-result-type ty)
    (if (proc-type? ty)
        (caddr ty)
        (eopl:error 'proc-type-result-type "Invalid proc type.")))

  ;; type-variable-counter : Int
  ;;
  ;; Private to this file.

  (define type-variable-counter 0)

  ;; type-variable-expressions : Listof[(Type * Exp)]
  ;;
  ;; Associates type variables with expressions, for error messages.
  ;; Private to this file.

  (define type-variable-expressions '())

  ;; Type constraints are an abstract data type, so no client code
  ;; should depend upon their representation, which is
  ;; subject to change without notice.
  ;;
  ;; For now, type constraints are represented by lists
  ;; as specified by the following grammar.
  ;;
  ;; <type>  ::=  (equal <type> <type>)
  ;;
  ;; This representation allows subtyping constraints to be
  ;; added later.

  ;; make-type-constraint : Type * Type -> TypeConstraint

  (define (make-type-constraint ty1 ty2)
    (list 'equal ty1 ty2))

  ;; type-constraint-lhs : TypeConstraint -> Type

  (define (type-constraint-lhs tc)
    (cadr tc))

  ;; type-constraint-rhs : TypeConstraint -> Type

  (define (type-constraint-rhs tc)
    (caddr tc))

  ;; equal-types? : Type * Type -> Boolean
  ;;
  ;; FIXME: assumes distinct type variables are not equal.

  (define (equal-types? t1 t2)
    (equal? t1 t2))

  ;; solve-type-constraints: Listof[TypeConstraint]
  ;;                             -> Maybe[Listof[TypeConstraint]]
  ;;
  ;; where Maybe[T] stands for { #f } union T.
  ;;
  ;; (solve-type-contraints tcs) returns a list of substitutions
  ;; if tcs is a solvable list of type contraints, and returns #f
  ;; if tcs has no solution.

  (define (solve-type-constraints tcs)
    ;(for-each (lambda (tc) (write tc) (newline))
    ;          tcs)
    (let ((result
           (call-with-current-continuation
            (lambda (return)
              (solve-constraints tcs
                                 '()
                                 (lambda ()
                                   (return #f)))))))
      (set! type-variable-counter 0)
      (set! type-variable-expressions '())
      result))

  ;; solve-constraints: Listof[TypeConstraint]
  ;;                             * Listof[TypeConstraint] * FailureContinuation
  ;;                             -> Maybe[Listof[TypeConstraint]]
  ;;
  ;; (solve-constraints tcs subst fail) returns a list of substitutions
  ;; consistent with tcs and subst if such a thing exists, or
  ;; calls fail with no arguments if no such thing exists.
  ;;
  ;; Invariants:
  ;;     Every type constraint in subst has a type variable as its lhs.
  ;;     No type variable that appears on the lhs of a type constraint
  ;;         in subst also appears on the rhs of a type constraint in
  ;;         subst.

  (define (solve-constraints tcs subst fail)
    (cond ((null? tcs)
           subst)
          (else
           (analyze-constraint (car tcs) (cdr tcs) subst fail))))

  ;; analyze-constraint: TypeConstraint
  ;;                         * Listof[TypeConstraint]
  ;;                         * Listof[TypeConstraint]
  ;;                         * FailureContinuation
  ;;                         -> Listof[TypeConstraint]
  ;;
  ;; (analyze-constraint tc tcs subst fail) returns a list of substitutions
  ;; consistent with (cons tc tcs) and subst if such a thing exists,
  ;; or calls fail with no arguments if no such thing exists.
  ;;
  ;; Invariants: same as for solve-constraints.

  (define (analyze-constraint tc tcs subst fail)
    ;(display "Analyzing...")
    ;(newline)
    ;(write tc)
    ;(newline)
    (let* ((tc (apply-substitution-to-constraint subst tc))
           (lhs (type-constraint-lhs tc))
           (rhs (type-constraint-rhs tc)))
      (cond ((equal-types? lhs rhs)
             (solve-constraints tcs subst fail))
            ((and (type-variable? lhs)
                  (type-variable? rhs))
             (analyze-constraint (make-type-constraint rhs lhs)
                                 tcs
                                 (extend-substitution subst tc fail)
                                 fail))
            ((type-variable? rhs)
             (analyze-constraint (make-type-constraint rhs lhs)
                                 tcs
                                 subst
                                 fail))
            ((type-variable? lhs)
             (solve-constraints tcs
                                (extend-substitution subst tc fail)
                                fail))
            ((and (int-type? lhs)
                  (int-type? rhs))
             (solve-constraints tcs subst fail))
            ((and (bool-type? lhs)
                  (bool-type? rhs))
             (solve-constraints tcs subst fail))
            ((and (proc-type? lhs)
                  (proc-type? rhs))
             (let ((lhs-argtypes (proc-type-argument-types lhs))
                   (rhs-argtypes (proc-type-argument-types rhs))
                   (lhs-result (proc-type-result-type lhs))
                   (rhs-result (proc-type-result-type rhs)))
               (if (= (length lhs-argtypes)
                      (length rhs-argtypes))
                   (solve-constraints (append (map make-type-constraint
                                                   lhs-argtypes
                                                   rhs-argtypes)
                                              (cons (make-type-constraint
                                                     lhs-result
                                                     rhs-result)
                                                    tcs))
                                      subst
                                      fail)
                   (fail))))
            (else (fail)))))

  ;; apply-substitution-to-constraint : Listof[TypeConstraint] * TypeConstraint
  ;;                                        -> TypeConstraint
  ;;
  ;; (apply-substitution-to-constraint subst tc) returns the result of applying
  ;; the substitutions in subst to tc.

  (define (apply-substitution-to-constraint subst tc)
    (make-type-constraint
     (apply-substitution subst (type-constraint-lhs tc))
     (apply-substitution subst (type-constraint-rhs tc))))

  ;; apply-substitution : Listof[TypeConstraint] * TypeConstraint
  ;;                          -> TypeConstraint
  ;;
  ;; (apply-substitution subst ty) returns the result of applying
  ;; the substitutions in subst to ty.

  (define (apply-substitution subst ty)
    (cond ((int-type? ty) ty)
          ((bool-type? ty) ty)
          ((proc-type? ty)
           (proc-type (map (lambda (argtype)
                             (apply-substitution subst argtype))
                           (proc-type-argument-types ty))
                      (apply-substitution subst
                                          (proc-type-result-type ty))))
          ((type-variable? ty)
           (let ((ty2 (substitution-lookup subst ty)))
             (if ty2
                 ty2
                 ty)))
          (else
           (eopl:error 'apply-substitution "This can't happen."))))

  ;; extend-substitution : Listof[TypeConstraint]
  ;;                          * TypeConstraint * FailureContinuation
  ;;                          -> TypeConstraint
  ;;
  ;; (extend-substitution subst tc fail) returns the result of
  ;; extending the substitutions in subst with tc, or calls fail
  ;; with no arguments if that would violate the invariants for
  ;; a substitution.
  ;;
  ;; pre-condition: no type variables defined by subst occur within tc,
  ;;     and the left hand side of tc is a type variable.

  (define (extend-substitution subst tc fail)
    (let ((lhs (type-constraint-lhs tc))
          (rhs (type-constraint-rhs tc)))
      (cond ((not (type-variable? lhs))
             (eopl:error 'extend-substitution "Lhs should be type variable."))
            ((equal-types? lhs rhs)
             subst)
            ((occurs-within? lhs rhs)
             (fail))
            (else
             (let ((smallsubst (list tc)))
               (cons tc
                     (map (lambda (tc)
                            (apply-substitution-to-constraint smallsubst tc))
                          subst)))))))

  ;; occurs-within? : TypeVariable * Type -> Boolean
  ;;
  ;; (occurs-within? tvar ty) returns true if and only if
  ;; tvar occurs within ty

  (define (occurs-within? tvar ty)
    (cond ((int-type? ty) #f)
          ((bool-type? ty) #f)
          ((proc-type? ty)
           (let loop ((argtypes (proc-type-argument-types ty)))
             (cond ((null? argtypes)
                    (occurs-within? tvar (proc-type-result-type ty)))
                   ((occurs-within? tvar (car argtypes))
                    #t)
                   (else
                    (loop (cdr argtypes))))))
          ((type-variable? ty)
           (equal-types? tvar ty))
          (else
           (eopl:error 'occurs-within? "This can't happen"))))

  ;; substitution-lookup : Listof[TypeConstraint] * TypeVariable
  ;;                          -> Maybe[TypeConstraint]
  ;;
  ;; (substitution-lookup subst tvar) returns the type associated with tvar
  ;; if it exists within subst, and returns #f otherwise.

  (define (substitution-lookup subst tvar)
    (cond ((null? subst)
           #f)
          ((equal? (type-constraint-lhs (car subst)) tvar)
           (type-constraint-rhs (car subst)))
          (else
           (substitution-lookup (cdr subst) tvar))))

  )
