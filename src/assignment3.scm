;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                ASSIGNMENT 3                                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Eliminate nested defines:

(define eliminate-nested-defines
  (lambda (pe)
    (cond ((atom? pe)
           pe)
          ((equal? (car pe) 'lambda-simple)
           `(,(car pe) ,(cadr pe) ,(handle-nested-defines (caddr pe))))
          ((equal? (car pe) 'lambda-opt)
           `(,(car pe) ,(cadr pe) ,(caddr pe) ,(handle-nested-defines (cadddr pe))))
          ((equal? (car pe) 'lambda-var)
           `(,(car pe) ,(cadr pe) ,(handle-nested-defines (caddr pe))))
          (else (cons (eliminate-nested-defines (car pe))
                      (eliminate-nested-defines (cdr pe)))))))
          ;(else (map eliminate-nested-defines pe)))))

(define handle-nested-defines
  (lambda (lambda-body)
    (split-defines-expressions
     (eliminate-nested-defines (list lambda-body))
     (lambda (ds es)
       (if (null? ds)
           (if (> (length es) 1)
               `(seq ,es)
               (car es))
           (let* ((assignments (map (lambda (def-exp) (cdr def-exp))
                                    ds))
                  (vars (get-vars assignments))
                  (vars-names (map (lambda (var) (cadr var))
                                   vars))
                  (default-assignments (map (lambda (v)
                                              `(const #f))
                                            vars))
                  (set-assignments (map (lambda (p)
                                          `(set ,(car p) ,(cadr p)))
                                        assignments)))
             `(applic
               (lambda-simple ,vars-names
                              (seq ,(append set-assignments es)))
               ,default-assignments)))))))

(define split-defines-expressions
  (lambda (pes ret-ds-es)
    (if (null? pes) (ret-ds-es '() '())
        (split-defines-expressions 
         (cdr pes)
         (lambda (ds es)
           (cond ((eq? (caar pes) 'def)
                  (ret-ds-es (cons (car pes) ds) es))
                 ((eq? (caar pes) 'seq)
                  (split-defines-expressions (cadar pes)
                                             (lambda (ds1 es1)
                                               (ret-ds-es (append ds1 ds)
                                                          (append es1 es)))))
                 (else (ret-ds-es ds (cons (car pes) es)))))))))


; Remove redundant applications

(define remove-applic-lambda-nil
  (let ((run
         (compose-patterns
          (pattern-rule
           `(applic (lambda-simple () ,(? 'body)) ())
           (lambda (body)
             (remove-applic-lambda-nil body))))))
    (lambda (exp)
      (run exp 
           (lambda ()
             (if (atom? exp)
                 exp
                 (cons (remove-applic-lambda-nil (car exp))
                       (remove-applic-lambda-nil (cdr exp)))
                 #;(map remove-applic-lambda-nil exp)))))))

; Boxing of variables

(define contains-var?
  (lambda (var)
    (lambda (lst)
      (member (cadr var) lst))))

(define tagged-var?
  (lambda (var)
    (and (list? var)
         (equal? 'var (car var)))))

(define var-equal?
  (lambda (arg var)
    (and (tagged-var? var)
         (equal? arg (cadr var)))))

(define deep-member?
  (lambda (x)
    (lambda (lst)
      (cond ((atom? lst) #f)
            ((equal? x lst) #t)
            ((member x lst)#t)
            (else (ormap (deep-member? x) lst))))))


(define box-set
  (lambda (pe)
    (cond ((atom? pe)
           pe)
          ((equal? (car pe) 'lambda-simple)
           `(,(car pe) ,(cadr pe) ,(apply-box-set (cadr pe) (box-set (caddr pe)))))
          ((equal? (car pe) 'lambda-opt)
           `(,(car pe) ,(cadr pe) ,(caddr pe) ,(apply-box-set (append (cadr pe) (list (caddr pe))) (box-set (cadddr pe)))))
          ((equal? (car pe) 'lambda-var)
           `(,(car pe) ,(cadr pe) ,(apply-box-set (list (cadr pe)) (box-set (caddr pe)))))
          (else (cons (box-set (car pe))
                      (box-set (cdr pe))))
          #;(else (map box-set pe)))))

(define apply-box-set
  (lambda (args body)
    (let ((box-set-assignments (map (lambda (x) `(set (var ,x) (box (var ,x))))
                                    (filter (lambda (x) (should-box? x body)) args))))
      (if (null? box-set-assignments)
          body
          `(seq ,(append box-set-assignments 
                         (if (equal? (car body) 'seq)
                             (box-body args (cadr body))
                             (list (box-body args body)))))))))


(define box-body
  (lambda (args body)
    (if (null? args)
        body
        (if (should-box? (car args) body)
            (box-body (cdr args)
                      ((box-replace-arg (car args)) body))
            (box-body (cdr args) body)))))

(define box-replace-arg
  (lambda (arg)
    (lambda (body)
      (cond ((atom? body)
             body)
            ((and (equal? (car body) 'var) (equal? (cadr body) arg))
             `(box-get ,body))
            ((and (equal? (car body) 'set) (equal? (cadadr body) arg))
             `(box-set ,(cadr body) ,((box-replace-arg arg) (caddr body))))
            ((equal? (car body) 'lambda-simple)
             `(,(car body) ,(cadr body) ,(if (member arg (cadr body))
                                             (caddr body)
                                             ((box-replace-arg arg) (caddr body)))))
            ((equal? (car body) 'lambda-opt)
             `(,(car body) ,(cadr body) ,(caddr body) ,(if (member arg (append (cadr body) (list (caddr body))))
                                                           (cadddr body)
                                                           ((box-replace-arg arg) (cadddr body)))))
            ((equal? (car body) 'lambda-var)
             `(,(car body) ,(cadr body) ,(if (equal? arg (cadr body))
                                             (caddr body)
                                             ((box-replace-arg arg) (caddr body)))))
            (else (map (box-replace-arg arg) body))))))


(define cut
  (lambda (var)
    (lambda (expr)
      (cond ((atom? expr) expr)
            ((and (equal? (car expr) 'lambda-simple)
                  ((contains-var? var) (cadr expr)))
             '())
            ((and (equal? (car expr) 'lambda-opt)
                  ((contains-var? var) (append (cadr expr) (list (caddr expr)))))
             '())
            ((and (equal? (car expr) 'lambda-var) (equal? (cadr var) (cadr expr)))
             '())
            (else (map (cut var) expr))))))


(define has-bound-occurrences?
  (lambda (var)
    (lambda (pe)
      (cond ((atom? pe) #f)
            ((equal? (car pe) 'lambda-simple)
             ((deep-member? var) (caddr pe)))
            ((equal? (car pe) 'lambda-opt)
             ((deep-member? var) (cadddr pe)))
            ((equal? (car pe) 'lambda-var)
             ((deep-member? var) (caddr pe)))
            (else (ormap (has-bound-occurrences? var) pe))))))

(define has-set-occurrences?
  (lambda (var)
    (lambda (pe)
      (cond ((atom? pe) #f)
            ((and (equal? (car pe) 'set) (equal? (cadr pe) var)) #t)
            (else (ormap (has-set-occurrences? var) pe))))))

(define has-get-occurrences?
  (lambda (var)
    (lambda (pe)
      (cond ((atom? pe) #f)
            ((equal? (car pe) 'set) ((has-get-occurrences? var) (caddr pe)))
            ((or (equal? var pe) (member var pe)) #t)
            (else (ormap (has-get-occurrences? var) pe))))))

(define should-box?
  (lambda (var pe)
    (let* ((var `(var ,var))
           (cutted-pe ((cut var) pe)))
      (and ((has-bound-occurrences? var) cutted-pe)
           ((has-set-occurrences? var) cutted-pe)
           ((has-get-occurrences? var) cutted-pe)
           ))))

; Annotating variables with their lexical address

(define get-index
  (lambda (item lst)
    (cond ((null? lst) -1)
          ((equal? item (car lst)) 0)
          ((equal? (get-index item (cdr lst)) -1) -1)
          (else (+ 1 (get-index item (cdr lst)))))))

(define in-scope?
  (lambda (obj scope)
    (ormap (lambda (lst) (member obj lst)) scope)))

(define get-major-index
  (lambda (obj scope)
    (if (member obj (car scope))
        0
        (+ 1 (get-major-index obj (cdr scope))))))

(define get-minor-index
  (lambda (obj major scope)
    (if (= major 0)
        (get-index obj (car scope))
        (get-minor-index obj (- major 1) (cdr scope)))))

(define pe->lex-pe
  (lambda (pe)
    (scan-lex pe '() '())))

(define scan-lex
  (lambda (expr scope vars)
    (cond ((atom? expr)
           expr)
          ((equal? (car expr) 'var)
           (let ((var (cadr expr)))
             (cond ((member var vars)
                    `(pvar ,var ,(get-index var vars)))
                   ((in-scope? (cadr expr) scope)
                    (let ((major (get-major-index var scope)))
                      `(bvar ,(cadr expr) ,major ,(get-minor-index var major scope))))
                   (else
                    `(fvar ,var)))))
          ((equal? (car expr) 'lambda-simple)
           (let ((args (cadr expr))
                 (body (cddr expr)))
             `(lambda-simple ,args
                             ,@(map (lambda (sub-expr)
                                      (scan-lex sub-expr (cons vars scope) args))
                                    body))))
          ((equal? (car expr) 'lambda-opt)
           (let ((args (cadr expr))
                 (opt-arg (caddr expr))
                 (body (cdddr expr)))
             `(lambda-opt ,args ,opt-arg
                          ,@(map (lambda (sub-expr)
                                   (scan-lex sub-expr (cons vars scope) (append args (list opt-arg))))
                                 body))))
          ((equal? (car expr) 'lambda-var)
           (let ((arg (cadr expr))
                 (body (cddr expr)))
             `(lambda-var ,arg
                          ,@(map (lambda (sub-expr)
                                   (scan-lex sub-expr (cons vars scope) (list arg)))
                                 body))))
          (else (cons (scan-lex (car expr) scope vars)
                      (scan-lex (cdr expr) scope vars)))
          #;(else
           (map (lambda (sub-expr)
                  (scan-lex sub-expr scope vars))
                expr)))
    ))


; Annotate tail calls

(define annotate-tc
  (lambda (pe)
    (scan-tc pe #f)))

(define scan-tc
  (lambda (expr tp)
    (cond ((or (atom? expr) (equal? (car expr) 'var) (equal? (car expr) 'const))
           expr)
          ((equal? (car expr) 'applic)
           (if tp
               `(tc-applic ,@(scan-tc (cdr expr) #f))
               `(applic ,@(scan-tc (cdr expr) #f))))
          ((equal? (car expr) 'or)
           `(,(car expr) ,(or-tc (cadr expr) tp)))
          ((equal? (car expr) 'if3)
           `(,(car expr) ,(scan-tc (cadr expr) #f) ,(scan-tc (caddr expr) tp) ,(scan-tc (cadddr expr) tp)))
          ((equal? (car expr) 'def)
           `(,(car expr) ,(cadr expr) ,(scan-tc (caddr expr) #f)))
          ((equal? (car expr) 'lambda-simple)
           `(,(car expr) ,(cadr expr) ,(scan-tc (caddr expr) #t)))
          ((equal? (car expr) 'lambda-opt)
           `(,(car expr) ,(cadr expr) ,(caddr expr) ,(scan-tc (cadddr expr) #t)))
          ((equal? (car expr) 'lambda-var)
           `(,(car expr) ,(cadr expr) ,(scan-tc (caddr expr) #t)))
          ((or (equal? (car expr) 'set) (equal? (car expr) 'box-set))
           `(,(car expr) ,(cadr expr) ,(scan-tc (caddr expr) #f)))
          ((equal? (car expr) 'seq)
           `(,(car expr) ,(seq-tc (cadr expr) tp)))
          
          (else (map (lambda (sub-expr) (scan-tc sub-expr tp)) expr)))))

(define last-expr-tc
  (lambda (expr tp)
    (if (null? (cdr expr))
        (cons (scan-tc (car expr) tp) '())
        (cons (scan-tc (car expr) #f) (or-tc (cdr expr) tp)))))

(define or-tc
  last-expr-tc)
(define seq-tc
  last-expr-tc)

(define f (lambda (x)
            (annotate-tc
             (pe->lex-pe
              (box-set
               (remove-applic-lambda-nil
                (eliminate-nested-defines
                 (parse x))))))))