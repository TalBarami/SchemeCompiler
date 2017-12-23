(load "src/assignment1.scm")
(load "src/assignment2.scm")
(load "src/assignment3.scm")
(load "src/assignment4.scm")

(define disp
  (lambda (x)
    (newline)
    (newline)
    (display x)
    (newline)
    (newline)))



(define tester (lambda(test)
                 (begin
                   (system "rm -f outResult")
                   (system "rm -f outFile")
                   (system "rm -f outFile.c")
                   (system "rm -f outFile.scm")
                   (string->file test "outFile.scm")
                   (compile-scheme-file "outFile.scm" "outResult.c")
                   (newline)(display test)
                   (newline) (display  `===>)
                   (system "gcc -o outFile outResult.c")
                   (system "./outFile > outResult")
                   (let ((result (file->string "outResult")))
                     (system "rm -f outResult")
                     (system "rm -f outFile")
                     (system "rm -f outFile.c")
                     (system "rm -f outFile.scm")
                     (display result))
                   (newline))))

(define t1
  "((((lambda (a b . c) (lambda (d e . f) (lambda (h i . j) j))) 1 2) 3 4) 5 6)")

(define t2
  "((((lambda (a b . c) (lambda (d e . f) (lambda (h i . j) j))) 1 2 3 4 5) 3 4 5) 5 6)")


(define t3
  "((((lambda (a b . c) (lambda (d . e) (lambda (f g h i j . k) f))) 1 2) 3 4) 5 6 7 8 9 10)"
  )

(define t4
  "((lambda (x y) (x y)) (lambda (x) x) 1)"
  )

(define t5
  "(((lambda (x) (lambda (y) (set! x 1) (set! y x) x)) 6) 7)"
  )