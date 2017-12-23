(define get-write
  (lambda (inst)
    (caddr inst)))

(define get-read
  (lambda (inst)
    (cadr inst)))

(define get-inst
  (lambda (inst)
    (car inst)))

(define has-read?
  (lambda (register lst)
    (ormap (lambda (inst)
             (member register (get-read inst)))
           lst)))

(define has-write?
  (lambda (register lst)
    (ormap (lambda (inst)
             (member register (get-write inst)))
           lst)))

(define has-read-write?
  (lambda (register lst)
    (ormap (lambda (inst)
             (and (member register (get-write inst))
                  (member register (get-read inst))))
           lst)))

(define has-empty-write?
  (lambda (inst)
    (null? (get-write inst))))

(define cut
  (lambda (register lst acc)
    (cond ((null? lst)
           acc)
          ((member register (get-write (car lst)))
           (cons (car lst) acc))
          (else (cut register (cdr lst) (cons (car lst) acc))))))

(define get-rest
  (lambda (register lst)
    (cond ((null? lst)
           '())
          ((member register (get-write (car lst)))
           (cdr lst))
          (else (get-rest register (cdr lst))))))

(define should-remove?
  (lambda (inst lst)
    (if (null? lst)
        #f
        (or (has-empty-write? inst)
            (andmap (lambda (register)
                      (let ((cutted (cut register lst '()))
                            (rest (get-rest register lst)))
                        (if (has-read-write? register cutted)
                            (should-remove? (car cutted) rest)
                            (and (not (has-read? register cutted))
                                 (or (has-write? register cutted)
                                     (should-remove? (car cutted) rest))))
                        ))
                    (get-write inst))
            ))))


(define remww
  (lambda (lst)
    (cond ((null? lst)
           '())
          ((should-remove? (car lst) (cdr lst))
           (remww (cdr lst)))
          (else (cons (car lst) (remww (cdr lst)))))))


(define t1
  '((f1 (1 2) (0))
    (f2 (3 4) (3))
    (f3 (1) (5))
    (f4 (3 4) (1))
    (f5 (3 6) (1))
    (f6 (0 9) (2))
    (f7 (1) ())
    ))

(define t2
  '((g46494 (6 1) (6))
    (g46495 (3 6 0) (0 4))
    (g46496 (1) (3))
    (g46497 (5 0 6) (4 2))
    (g46498 (7 1 3) ())
    (g46499 (7) ())
    (g46500 (0 1) ())
    (g46501 (4 7) (7))
    (g46502 (5 7 0) ())
    (g46503 (5 2 0) (6 2))
    (g46504 (6 4 5) (0))
    (g46505 (1 2) (1))
    (g46506 (6 2 7 5) (7))
    (g46507 (1) ())
    (g46508 (1) ())
    (g46509 (0 3 2) (5 0))
    (g46510 (3 6) (4))
    ))

(define t3
  '((f1 (0) (1 2 3))
    (f2 (0) (1))
    (f3 (0) (2))
    (f4 (0) (2 3))
    (f5 (3) (3))
    (f6 (0) (3))
    (f7 (0) (3))
    (f8 (0) (3))))

(define t4
  '((i1 (1) (1))
    (i2 (1) (1))
    (i3 () (1))))

(define t5
  '((f1 (6 1) (9))
    (f2 (3 6 0) (0 4))
    (f3 (1) (3))
    (f4 (9) (4 2))
    (f5 (7 1 3) (9))
    (f6 (7) (9))))

(define t6
  '((g46494 (6 1) (6))
    (g46494 (6 1) (6))
    (g46494 (6 1) (6))
    (g46494 (6 1) (6))
    (g46494 (6 1) (6))
    (g46494 (7) (6))))