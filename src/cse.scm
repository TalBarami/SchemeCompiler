(load "src/pattern-matcher.scm")

(print-gensym #f)

(define *reserved-words*
  '(quasiquote unquote unquote-splicing quote))

(define not-reserved-word?
  (lambda (exp)
    (not (member exp *reserved-words*))))

(define applic?
  (let ((run
         (compose-patterns
          (pattern-rule
           `(,(? 'apply not-reserved-word?) . ,(? 'exprs list?))
           (lambda (apply exprs)
             (cons apply exprs))))))
    (lambda (exp)
      (run exp 
           (lambda () #f)))))

(define flatten
  (lambda (lst)
    (cond ((null? lst) '())
          ((pair? lst) (append (flatten (car lst)) (flatten (cdr lst))))
          (else (list lst)))))

(define count
  (lambda (item lst)
    (length
     (filter (lambda (x)(equal? item x))
             lst))))

(define remove-occurrences
  (lambda (lst)
    (if (null? lst) '()
        (cons (car lst) (remove-occurrences
                         (filter (lambda (x) (not (equal? x (car lst))))
                                 (cdr lst)))))))

(define get-duplicates
  (lambda (lst)
    (remove-occurrences (filter (lambda (x)
                                  (> (count x lst) 1))
                                lst))))

(define deep-count
  (lambda (item lst)
    (fold-left (lambda (x y)
                 (+ x
                    (cond ((atom? y) 0)
                          ((equal? item y) 1)
                          (else (deep-count item y)))))
               0
               lst)))

(define collect-applications
  (lambda (lst)
    (if (null? lst)
        '()
        (let ((first (car lst))
              (rest (cdr lst)))
          (if (applic? first)
              (append `(,first) (collect-applications first) (collect-applications rest))
              (collect-applications rest))))))

(define filter-apps
  (lambda (exp apps-lst)
    (cond ((null? exp) '())
          ((atom? (car exp)) (filter-apps (cdr exp) apps-lst))
          ((member (car exp) apps-lst) (cons (car exp) (filter-apps (cdr exp) apps-lst)))
          (else (append (filter-apps (car exp) apps-lst) (filter-apps (cdr exp) apps-lst))))))

(define order-function
  (lambda (x y) (< (length (flatten x)) (length (flatten y)))))

(define filter-non-repetetive
  (lambda (apps-lst)
    (get-duplicates (filter (lambda (x) (> (length x) 1)) (collect-applications apps-lst)))))

(define get-applications
  (lambda (lst)
    (let* ((sorted-apps (sort order-function (get-duplicates (collect-applications lst))))
           (filtered-apps (remove-occurrences (filter-apps lst sorted-apps)))
           (filtered-non-repetetive-apps (filter-non-repetetive filtered-apps)))
      (sort order-function
            (remove-occurrences (append filtered-apps filtered-non-repetetive-apps))))))

(define f get-applications)

(define map-applications
  (lambda (lst)
    (map (lambda (x) (cons x (gensym)))
         lst)))

(define get-val
  (lambda (key mapped-vars)
    (cdar (filter (lambda (x) (equal? key (car x)))
                  mapped-vars))))

(define map-contains
  (lambda (item mapped-vars)
    (member item (map (lambda (x) (car x))
                      mapped-vars))))

(define replace
  (lambda (exp mapped-vars)
    (map (lambda (x) (cond ((atom? x) x)
                           ((map-contains x mapped-vars) (get-val x mapped-vars))
                           (else (replace x mapped-vars))))
         exp)))

(define map-replace
  (lambda (exp mapped-vars)
    (map (lambda (x)
           (cond ((equal? (car x) exp) x)
                 (else (cons (replace (car x) mapped-vars) (cdr x)))))
         mapped-vars)))

(define get-let-assignments
  (lambda (mapped-vars)
    (map (lambda (x) (list (cdr x) (car x)))
         mapped-vars)))

(define apply-cse
  (lambda (exp)
    (let* ((mapped (map-applications (get-applications exp)))
           (let-assignments (get-let-assignments (map-replace exp mapped)))
           (let-body (replace exp mapped)))
      (cond ((= (length mapped) 0) let-body)
            ((= (length mapped) 1) `(let ,let-assignments ,let-body))
            (else `(let* ,let-assignments ,let-body))))))

(define cse apply-cse)