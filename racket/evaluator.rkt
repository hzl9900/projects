#lang sicp

(define (atom? x)
  (not (pair? x)))

(define (tagged? tag exp)
  (eq? tag (car exp)))


(define (enclosing-env env) (cdr env))
(define (first-frame env) (car env))
(define (make-frame vars vals)
  (cons vars vals))
(define (frame-vars frame) (car frame))
(define (frame-vals frame) (cdr frame))
(define (add-binding-to-frame var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (extend-env vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (else "dismatch -- EXTEND-ENV")))
(define (lookup-var-val var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-env env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env empty-env)
        (error "empty env")
        (let ((frame (first-frame env)))
          (scan (frame-vars frame)
                (frame-vals frame)))))
  (env-loop))
(define (set-var-val! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-env env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env empty-env)
        (error "ERR")
        (let ((frame (first-frame env)))
          (scan (frame-vars frame)
                (frame-vals frame)))))
  (env-loop env))
(define (define-var! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-vars frame)
          (frame-vals frame))))

(define empty-env '())

(define (true? x)
  (not (eq? x #f)))
(define (false? x)c
  (eq? x #f))

(define (make-proc params body env)
  (list 'procedure params body env))
(define (compound-proc? p)
  (tagged? 'procedure p))
(define (proc-params p) (cadr p))
(define (proc-body p) (caddr p))
(define (proc-env p) (cadddr p))


; representation of expr

(define (self-evaluating? exp)
  (or (number? exp)
      (symbol? exp)))

; (quote <text-of-quotation>)

(define (quoted? exp)
  (tagged? 'quote exp))
(define (text-of-quotation exp)
  (cadr exp))

(define (var? exp)
  (symbol? exp))

; (set! <var> <val>)

(define (assignment? exp)
  (tagged? 'set! exp))
(define (assignment-var exp)
  (cadr exp))
(define (assignment-val exp)
  (caddr exp))

; (define <var> <val>)
; (define (<var> <param1> ... <paramn>) <body>)

(define (definition? exp)
  (tagged? 'define exp))
(define (definition-var exp)
  (if (atom? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-val exp)
  (if (atom? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

; (lambda (<params>) <body>)

(define (lambda? exp) (tagged? 'lambda exp))
(define (lambda-params exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda params body)
  (cons 'lambda (cons params body)))

; (if <pred> <csqt> <alt>)

(define (if? exp) (tagged? 'if exp))
(define (if-pred exp) (cadr exp))
(define (if-csqt exp) (caddr exp))
(define (if-alt exp)
  (if (not (null? (caddr exp)))
      (cadddr exp)
      'false))
(define (make-if pred csqt alt)
  (list 'if pred csqt alt))

; (begin (<actions>))

(define (begin? exp) (tagged? 'begin exp))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr exp)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr exp))
(define (seq->exp seq)
  (cond ((null? seq) seq)
        ((last-exp seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

; (oprt oprnd1 oprnd2 ...)

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; (cond clauses)

(define (cond? exp) (tagged 'cond exp))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-pred clause) 'else))
(define (cond-pred clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (seq->exp (cond-actions first))
                (error "ELSE clauses -- COND->IF" clauses))
            (make-if (cond-pred first)
                     (seq->exp (cond-actions first))
                     (expand-clause rest))))))

(define (eval exp env)
  (cond ((number? exp) exp)
        ((quoted? exp) (text-of-quotation exp))
        ((var? exp) (lookup-var-val exp))
        (else ("error -- EVAL"))))

(define (apply proc args)
  (cond ((primitive-proc? proc)
         (apply-primitive-proc proc args))
        ((compound-proc? proc)
         (eval-seq (proc-body proc)
                   (extend-env (proc-params proc)
                               args
                               (proc-env proc))))
        (else ("error -- APPLY"))))

        (eval '100 empty-env)
        (eval ''a empty-env)