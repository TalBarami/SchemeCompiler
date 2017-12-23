;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                ASSIGNMENT 2                                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "src/pattern-matcher.scm")
(load "src/qq.scm")

(define *reserved-words*
  '(and begin cond define do else if lambda
        let let* letrec or quasiquote unquote
        unquote-splicing quote set!))

(define *void-object*
  (if #f #f))

(define begin-exp? (lambda (exp) (equal? exp 'begin)))

(define filter-begin
  (lambda (lst)
    (fold-right (lambda (x y)
                  (if (list? x)
                      (if (equal? (car x) 'begin)
                          (filter-begin (append (cdr x) y))
                          (cons x y))
                      (cons x y)))
                '()
                lst)))

(define beginify
  (lambda (s)
    (cond
      ((null? s) *void-object*)
      ((null? (cdr s)) (car s))
      (else `(begin ,@(filter-begin s))))))

(define flatten
  (lambda (lst)
    (cond ((null? lst) '())
          ((pair? lst) (append (flatten (car lst)) (flatten (cdr lst))))
          (else (list lst)))))

(define remove-duplicates
  (lambda (lst)
    (if (null? lst) '()
        (cons (car lst) (remove-duplicates
                         (filter (lambda (x) (not (equal? x (car lst))))
                                 (cdr lst)))))))

(define get-vars
  (lambda (lst)
    (map (lambda (x) (car x)) lst)))

(define get-vals
  (lambda (lst)
    (map (lambda (x) (cadr x)) lst)))

(define void?
  (lambda (x)
    (equal? *void-object* x)))

(define not-reserved-word?
  (lambda (x)
    (not (member x *reserved-words*))))

(define simple-const?
  (lambda (x)
    (or (boolean? x)
        (char? x)
        (number? x)
        (string? x)
        (void? x))))

(define var?
  (lambda (x)
    (and (symbol? x)
         (not-reserved-word? x))))

(define vars?
  (lambda (lst)
    (andmap var? (flatten lst))))

(define differentiated-vars?
  (lambda (lst)
    (let ((flatten-list (flatten lst)))
      (and (vars? flatten-list)
           (= (length flatten-list)
              (length (remove-duplicates flatten-list)))))))

(define identify-lambda
  (lambda (argl ret-simple ret-opt ret-var)
    (cond 
      ((null? argl) (ret-simple '()))
      ((var? argl) (ret-var argl))
      (else (identify-lambda (cdr argl)
                             (lambda (s) (ret-simple `(,(car argl) ,@s)))
                             (lambda (s opt) (ret-opt `(,(car argl) ,@s) opt))
                             (lambda (var) (ret-opt `(,(car argl)) var)))))))

(define letrec-default-assignments
  (lambda (vars)
    (map (lambda (v)
           (list v #f))
         vars)))

(define letrec-set-assignments
  (lambda (assignments)
    (map (lambda (p)
           `(set! ,(car p) ,(cadr p)))
         assignments)))

(define apply-parse
  (let ((run
         (compose-patterns
          
          ;; constants:
          (pattern-rule
           (? 'c simple-const?)
           (lambda (c) `(const ,c)))
          
          (pattern-rule
           `(quote ,(? 'c))
           (lambda (c) `(const ,c)))
          
          ;; variables:
          (pattern-rule
           (? 'v var?)
           (lambda (v) `(var ,v)))
          
          ;; conditionals:
          (pattern-rule
           `(if ,(? 'test) ,(? 'dit))
           (lambda (test dit)
             `(if3 ,(apply-parse test) ,(apply-parse dit) ,(apply-parse *void-object*))))
          
          (pattern-rule
           `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
           (lambda (test dit dif)
             `(if3 ,(apply-parse test) ,(apply-parse dit) ,(apply-parse dif))))
          
          ;; disjunctions:
          (pattern-rule
           `(or)
           (lambda () (apply-parse (or))))
          
          (pattern-rule
           `(or ,(? 'expr) . ,(? 'exprs list?))
           (lambda (expr exprs)
             (if (null? exprs)
                 (apply-parse expr)
                 `(or ,(map apply-parse (cons expr exprs))))))
          
          ;; lambda forms:
          (pattern-rule
           `(lambda ,(? 'args differentiated-vars?) ,(? 'expr) . ,(? 'exprs list?))
           (lambda (args expr exprs)
             `(,@(identify-lambda args
                                  (lambda (s) `(lambda-simple ,s))
                                  (lambda (req opt) `(lambda-opt ,req ,opt))
                                  (lambda (v) `(lambda-var ,v)))
               ,(apply-parse (beginify (cons expr exprs))))))
          
          ;; definitions:
          (pattern-rule
           `(define ,(? 'var var?) ,(? 'expr) . ,(? 'exprs list?))
           (lambda (var expr exprs)
             `(def ,(apply-parse var) ,(apply-parse (beginify (cons expr exprs))))))
          
          (pattern-rule
           `(define ,(? 'var-and-args vars?) ,(? 'expr) . ,(? 'exprs list?))
           (lambda (var-and-args expr exprs)
             (let ((var (car var-and-args))
                   (args (cdr var-and-args)))
               `(def ,(apply-parse var) ,(apply-parse `(lambda ,args ,@(cons expr exprs)))))))
          
          ;; assignments
          (pattern-rule
           `(set! ,(? 'var var?) ,(? 'expr))
           (lambda (var expr)
             `(set ,(apply-parse var) ,(apply-parse expr))))
          
          ;; application
          (pattern-rule
           `(,(? 'apply not-reserved-word?) . ,(? 'exprs list?))
           (lambda (apply exprs)
             `(applic ,(apply-parse apply) ,(map apply-parse exprs))))
          
          ;; sequesnces:
          (pattern-rule
           `(begin)
           (lambda () (apply-parse *void-object*)))
          
          (pattern-rule
           `(begin ,(? 'expr) . ,(? 'exprs list?))
           (lambda (expr exprs)
             (if (null? exprs)
                 (apply-parse expr)
                 `(seq ,(map apply-parse (filter-begin (cons expr exprs)))))))
          
          ;; and:
          (pattern-rule
           `(and)
           (lambda () (apply-parse (and))))
          
          (pattern-rule
           `(and ,(? 'expr) . ,(? 'exprs list?))
           (lambda (expr exprs)
             (if (null? exprs)
                 (apply-parse expr)
                 (apply-parse `(if ,expr (and ,@exprs) #f)))))
          
          ;; cond:
          (pattern-rule
           `(cond (else ,(? 'expr) . ,(? 'exprs list?)))
           (lambda (expr exprs)
             (apply-parse (beginify (cons expr exprs)))))
          
          (pattern-rule
           `(cond (,(? 'pred) ,(? 'expr) . ,(? 'exprs list?)) . ,(? 'rest list?))
           (lambda (pred expr exprs rest)
             (if (null? rest) 
                 (apply-parse `(if ,pred ,(beginify (cons expr exprs))))
                 (apply-parse `(if ,pred ,(beginify (cons expr exprs)) (cond ,@rest))))))
          
          ;; let:
          (pattern-rule
           `(let ,(? 'assignments) ,(? 'expr) . ,(? 'exprs list?))
           (lambda (assignments expr exprs)
             (let ((vars (get-vars assignments))
                   (vals (get-vals assignments)))
               (apply-parse `((lambda ,vars ,@(cons expr exprs)) ,@vals)))))
          
          ;; let*:
          (pattern-rule
           `(let* () ,(? 'expr) . ,(? 'exprs list?))
           (lambda (expr exprs)
             (apply-parse `(let () ,@(cons expr exprs)))))
          
          (pattern-rule
           `(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest)) . ,(? 'exprs))
           (lambda (var val rest exprs)
             (if (null? rest)
                 (apply-parse `(let ((,var ,val)) ,@exprs))
                 (apply-parse `(let ((,var ,val))
                                 (let* ,rest . ,exprs))))))
          
          ;; letrec:
          (pattern-rule
           `(letrec () ,(? 'expr) . ,(? 'exprs list?))
           (lambda (expr exprs)
             (apply-parse `(let () ((lambda () ,@(cons expr exprs)))))))
          
          (pattern-rule
           `(letrec ,(? 'assignments) ,(? 'expr) . ,(? 'exprs list?))
           (lambda (assignments expr exprs)
             (let* ((vars (get-vars assignments))
                    (vals (get-vals assignments))
                    (default-assignments (letrec-default-assignments vars))
                    (actual-assignments (letrec-set-assignments assignments)))
               (apply-parse `(let ,default-assignments
                               (begin ,@actual-assignments
                                      ((lambda () ,@(cons expr exprs)))))))))
          
          ;; quasiquote:
          (pattern-rule
           `(,(string->symbol "quasiquote") ,(? 'expr))
           (lambda (expr)
             (apply-parse (expand-qq expr))))
          
          )))
    (lambda (e)
      (run e
           (lambda ()
             (error 'parse
                    (format "Unknown form: ~s" e)))))))

(define parse apply-parse)
