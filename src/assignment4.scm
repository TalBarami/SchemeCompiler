(define T_VOID 937610)
(define T_NIL 722689)
(define T_BOOL 741553)
(define T_CHAR 181048)
(define T_INTEGER 945311)
(define T_FRACTION 451794)
(define T_STRING 799345)
(define T_SYMBOL 368031)
(define T_PAIR 885397)
(define T_VECTOR 335728)
(define T_CLOSURE 276405)

(define nl (list->string (list #\newline)))
(define tab (list->string (list #\tab)))

(define file->string
  (lambda (in-file)
    (let ((in-port (open-input-file in-file)))
      (letrec ((run
                (lambda ()
                  (let ((ch (read-char in-port)))
                    (if (eof-object? ch)
                        (begin
                          (close-input-port in-port)
                          '())
                        (cons ch (run)))))))
        (list->string
         (run))))))

(define string->file
  (lambda (str out-file)
    (let ((out-port (open-output-file out-file 'truncate))
          (lst (string->list str)))
      (letrec ((run
                (lambda (lst)
                  (if (null? lst)
                      (close-output-port out-port)
                      (begin (write-char (car lst) out-port)
                             (run (cdr lst)))))))
        (run lst)))))


(define with
  (lambda (s f) (apply f s)))

(define tagged-by
  (lambda (tag pe)
    (equal? (car pe) tag)))

(define ^^label
  (lambda (name)
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        (string-append name (number->string n)))
      )))

(define string->sexprs
  (lambda (str)
    (let ((lst (string->list str)))
      (letrec ((helper
                (lambda (lst)
                  (<sexpr> lst 
                           (lambda (e s) (if (null? s)
                                             (list e)
                                             (cons e (helper s))))
                           (lambda (w) `(error ,@w))))))
        (helper lst)))))

(define sexpr-parse
  (lambda (sexpr)
    (annotate-tc (pe->lex-pe (box-set (remove-applic-lambda-nil (eliminate-nested-defines (parse sexpr))))))))

(define c-table '())
(define f-table '())

(define compile-scheme-file
  (lambda (s-file t-file)
    (let* ((str-content (file->string s-file))
           (s-exprs (string->sexprs str-content))
           (s-exprs-with-base-procedures (append scheme-funcs s-exprs))
           (parsed-exp (map sexpr-parse s-exprs-with-base-procedures)))
      (set! c-table (^c-table parsed-exp))
      (set! f-table (^f-table parsed-exp (find-current-mem-index)))
      (let* ((generated-code (map (lambda (x)
                                    (string-append (code-gen x)
                                                   (code-gen-print)))
                                  parsed-exp))
             (code-to-compile (string-append prolog
                                             (code-gen-c-table c-table)
                                             (code-gen-f-table f-table)
                                             (code-gen-sym-list c-table)
                                             (code-gen-primitives-closures f-table)
                                             (fold-left string-append "" generated-code)
                                             epilogue)))
        (string->file code-to-compile t-file)
        ))))

(define code-gen
  (lambda (pe)
    (cond ((tagged-by 'const pe) (code-gen-const (cadr pe)))
          ((tagged-by 'pvar pe) (code-gen-pvar (caddr pe)))
          ((tagged-by 'bvar pe) (code-gen-bvar (cddr pe)))
          ((tagged-by 'fvar pe) (code-gen-fvar (cadr pe)))
          ((tagged-by 'if3 pe) (code-gen-if3 (cdr pe)))
          ((tagged-by 'or pe) (code-gen-or (cadr pe)))
          ((tagged-by 'seq pe) (code-gen-seq (cadr pe)))
          ((tagged-by 'lambda-simple pe) (code-gen-lambda-simple (cdr pe)))
          ((tagged-by 'lambda-opt pe) (code-gen-lambda-opt (cdr pe)))
          ((tagged-by 'lambda-var pe) (code-gen-lambda-var (cdr pe)))
          ((tagged-by 'def pe) (code-gen-def (cdr pe)))
          ((tagged-by 'applic pe) (code-gen-applic (cdr pe)))
          ((tagged-by 'tc-applic pe) (code-gen-tc-applic (cdr pe)))
          ((tagged-by 'set pe) (code-gen-set (cdr pe)))
          ((tagged-by 'box pe) (code-gen-box (cadr pe)))
          ((tagged-by 'box-get pe) (code-gen-box-get (cadr pe)))
          ((tagged-by 'box-set pe) (code-gen-box-set (cdr pe)))
          (else (code-gen-error "Error in code-gen.")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;         print         ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ^label-return-void (^^label "L_return_void"))

(define code-gen-print
  (lambda ()
    (let ((return-void (^label-return-void)))
      (string-append
       tab "CMP(R0, IMM(SOB_VOID));" nl
       tab "JUMP_EQ(" return-void ");" nl
       tab "PUSH(R0);" nl
       tab "CALL(WRITE_SOB);" nl
       tab "DROP(1);" nl
       tab "CALL(NEWLINE);" nl
       tab return-void ":" nl
       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;         const         ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-gen-const
  (lambda (pe)
    (string-append
     "//const" nl
     tab "MOV(R0, IMM(" (number->string (get-address c-table pe)) "));" nl
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;          vars         ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-gen-pvar
  (lambda (minor)
    (string-append
     "//pvar" nl
     tab "MOV(R0, FPARG(" (number->string (+ minor 2)) "));" nl
     )))

(define code-gen-bvar
  (lambda (pe)
    (with pe
          (lambda (major minor)
            (string-append
             "//bvar" nl
             tab "MOV(R0, FPARG(IMM(0)));" nl
             tab "MOV(R0, INDD(R0, IMM(" (number->string major) ")));" nl
             tab "MOV(R0, INDD(R0, IMM(" (number->string minor) ")));" nl
             )))))

(define code-gen-fvar
  (lambda (var)
    (string-append
     "//fvar" nl
     tab "MOV(R0, IND(" (number->string (cadr (assoc var f-table))) "));" nl
     tab "CMP(R0, 0);" nl
     tab "JUMP_EQ(ERROR_UNDEFINED_FVAR);" nl
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;          if3          ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ^label-if3-else (^^label "L_if3_else"))
(define ^label-if3-exit (^^label "L_if3_exit"))
(define code-gen-if3
  (lambda (pe)
    (with pe
          (lambda (test dit dif)
            (let ((code-test (code-gen test))
                  (code-dit (code-gen dit))
                  (code-dif (code-gen dif))
                  (label-else (^label-if3-else))
                  (label-exit (^label-if3-exit)))
              (string-append
               "//if3" nl
               code-test nl
               tab "CMP(R0, IMM(SOB_FALSE));" nl
               tab "JUMP_EQ(" label-else ");" nl
               code-dit nl
               tab "JUMP(" label-exit ");" nl
               label-else ":" nl
               code-dif nl
               label-exit ":" nl
               ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;           or          ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ^label-or-exit (^^label "L_or_exit"))
(define code-gen-or
  (lambda (pe)
    (letrec ((label-exit (^label-or-exit))
             (^or
              (lambda (pe acc)
                (cond ((null? pe) acc)
                      ((null? (cdr pe))
                       (string-append
                        (code-gen (car pe)) nl
                        label-exit ":"))
                      (else
                       (string-append
                        (code-gen (car pe)) nl
                        tab "CMP(R0, IMM(SOB_FALSE));" nl
                        tab "JUMP_NE(" label-exit ");" nl
                        (^or (cdr pe) acc)))))))
      (^or pe (string-append "//or" nl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;          seq          ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-gen-seq
  (lambda (pe)
    (fold-left (lambda (res sub-pe) (string-append res (code-gen sub-pe) nl))
               (string-append "//seq" nl)
               pe)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;             lambda            ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ^label-lambda-start-vectors-loop (^^label "L_lambda_start_vectors_loop"))
(define ^label-lambda-exit-vectors-loop (^^label "L_lambda_exit_vectors_loop"))
(define ^label-lambda-start-args-loop (^^label "L_lambda_start_args_loop"))
(define ^label-lambda-exit-args-loop (^^label "L_lambda_exit_args_loop"))
(define ^label-lambda-start-collect-args-loop (^^label "L_lambda_start_collect_args_loop"))
(define ^label-lambda-exit-collect-args-loop (^^label "L_lambda_exit_collect_args_loop"))

(define ^label-lambda-body (^^label "L_lambda_body"))
(define ^label-lambda-exit (^^label "L_lambda_exit"))
(define ^label-lambda-empty-opt (^^label "L_lambda_empty_opt"))
(define ^label-lambda-reposition-args (^^label "L_lambda_reposition_args"))
(define ^label-lambda-reposition-done (^^label "L_lambda_reposition_done"))

(define enclosed-closures 0)

(define code-gen-lambda-simple
  (lambda (pe)
    (with pe
          (lambda (args body)
            (let ((lambda-exit (^label-lambda-exit)))
              (string-append
               "//code for lambda simple" nl
               (code-gen-lambda-prefix lambda-exit) nl
               
               "//check for invalid args count" nl
               tab "CMP(FPARG(1), " (number->string (length args)) ");" nl
               tab "JUMP_NE(ERROR_LAMBDA_INVALID_ARGS_COUNT);" nl nl
               
               (code-gen-lambda-suffix body lambda-exit))
              )))))

(define code-gen-lambda-opt
  (lambda (pe)
    (with pe
          (lambda (args opt body)
            (let ((lambda-exit (^label-lambda-exit)))
              (string-append
               "//code for lambda opt" nl
               (code-gen-lambda-prefix lambda-exit) nl
               
               (code-gen-lambda-fix-stack (length args)) nl
               
               (code-gen-lambda-suffix body lambda-exit))
              )))))

(define code-gen-lambda-var
  (lambda (pe)
    (with pe
          (lambda (var body)
            (let ((lambda-exit (^label-lambda-exit)))
              (string-append
               "//code for lambda var" nl
               (code-gen-lambda-prefix lambda-exit) nl
               
               (code-gen-lambda-fix-stack 0) nl
               
               (code-gen-lambda-suffix body lambda-exit)
               ))))))

(define code-gen-lambda-prefix
  (lambda (lambda-exit)
    (let ((copy-vectors-loop-start (^label-lambda-start-vectors-loop))
          (copy-vectors-loop-end (^label-lambda-exit-vectors-loop))
          (copy-args-loop-start (^label-lambda-start-args-loop))
          (copy-args-loop-end (^label-lambda-exit-args-loop))
          (body-start (^label-lambda-body)))
      (string-append
       "//init-new-lambda-closure" nl
       "//R1 <- FPARG(0), env" nl
       tab "MOV(R1, FPARG(0));" nl
       "//R2 <- MALLOC(1 + enclosed-closures)" nl
       tab "PUSH(IMM(" (number->string (+ 1 enclosed-closures)) "));" nl
       tab "CALL(MALLOC);" nl
       tab "DROP(1);" nl
       tab "MOV(R2, R0);" nl nl
       
       "//copy previous envs" nl
       tab "MOV(R4, IMM(0));" nl ; i
       tab "MOV(R5, IMM(1));" nl ; j
       tab copy-vectors-loop-start ":" nl
       tab "CMP(R4, " (number->string enclosed-closures) ");" nl
       tab "JUMP_GE(" copy-vectors-loop-end ");" nl
       tab "MOV(INDD(R2,R5), INDD(R1, R4));" nl
       tab "INCR(R4);" nl
       tab "INCR(R5);" nl
       tab "JUMP(" copy-vectors-loop-start ");" nl
       tab copy-vectors-loop-end ":" nl nl
       
       "//R3 <- FPARG(1), args count" nl
       tab "MOV(R3, FPARG(1));" nl
       tab "PUSH(R3);" nl
       tab "CALL(MALLOC);" nl
       tab "DROP(1);" nl
       tab "MOV(INDD(R2,IMM(0)), R0);" nl
       "//copy args from stack to the new env" nl
       tab "MOV(R4, IMM(0));" nl ; i
       tab "MOV(R5, IMM(2));" nl ; j
       tab copy-args-loop-start ":" nl
       tab "CMP(R4, R3);" nl
       tab "JUMP_GE(" copy-args-loop-end ");" nl
       tab "MOV(R6, INDD(R2, 0));"
       tab "MOV(INDD(R6, R4), FPARG(R5));" nl
       tab "INCR(R4);" nl
       tab "INCR(R5);" nl
       tab "JUMP(" copy-args-loop-start ");" nl
       tab copy-args-loop-end ":" nl nl
       
       "//build T_CLOSURE SOB" nl
       tab "PUSH(IMM(3));" nl
       tab "CALL(MALLOC);" nl
       tab "DROP(1);" nl
       tab "MOV(INDD(R0, 0), IMM(" (number->string T_CLOSURE) "));" nl
       tab "MOV(INDD(R0, 1), R2);" nl
       tab "MOV(INDD(R0, 2), LABEL(" body-start "));" nl
       tab "JUMP(" lambda-exit ");" nl
       tab body-start ":" nl
       tab "PUSH(FP);" nl
       tab "MOV(FP, SP);" nl
       ))))

(define code-gen-lambda-fix-stack
  (lambda (args-length)
    (string-append
     (let ((empty-opt (^label-lambda-empty-opt))
           (collect-args-loop-start (^label-lambda-start-collect-args-loop))
           (upper-reposition-args (^label-lambda-reposition-args))
           (lower-reposition-args (^label-lambda-reposition-args))
           (reposition-done (^label-lambda-reposition-done)))
       (string-append
        tab "MOV(R1, IMM(SOB_NIL));" nl
        "//check if there is a need to fix the stack" nl
        tab "CMP(FPARG(1), " (number->string args-length) ");" nl
        tab "JUMP_EQ(" empty-opt ");" nl
        tab "JUMP_LT(ERROR_LAMBDA_INVALID_ARGS_COUNT);" nl
        tab "MOV(R2, FPARG(1) + 1)" nl
        tab collect-args-loop-start ":" nl
        tab "PUSH(R1);" nl
        tab "PUSH(FPARG(R2));" nl
        tab "CALL(MAKE_SOB_PAIR);" nl
        tab "DROP(2);" nl
        tab "MOV(R1, R0);" nl
        tab "DECR(R2);" nl
        tab "CMP(R2, " (number->string (+ args-length 1)) ");" nl
        tab "JUMP_GT(" collect-args-loop-start ");" nl
        
        tab "//reposition each item in the current frame" nl
        tab "MOV(R3, FPARG(1));" nl
        tab "MOV(R4, " (number->string (+ args-length 1)) ");" nl
        tab "MOV(R5, R3 - R4);" nl
        tab upper-reposition-args ":" nl
        tab "MOV(FPARG(R3), FPARG(R4));" nl
        tab "DECR(R3);" nl
        tab "DECR(R4);" nl
        tab "CMP(R3, -2);" nl
        tab "JUMP_GE(" upper-reposition-args ");" nl
        tab "SUB(FP, R5);" nl
        tab "SUB(SP, R5);" nl
        tab "JUMP(" reposition-done ");" nl
        
        tab empty-opt ":" nl           
        tab "//reposition each item in the current frame, reducing position by 1" nl
        tab "MOV(R3, 0);" nl
        tab "MOV(R4, -1);" nl
        tab lower-reposition-args ":" nl
        tab "MOV(FPARG(R4 - 2), FPARG(R3 - 2));" nl
        tab "INCR(R3);" nl
        tab "INCR(R4);" nl
        tab "CMP(R3," (number->string (+ args-length 4)) ");" nl
        tab "JUMP_LT(" lower-reposition-args ");" nl
        tab "INCR(FP);" nl
        tab "INCR(SP);" nl
        
        tab reposition-done ":" nl
        
        "//replace the opt arg with the newly created list" nl
        tab "MOV(FPARG(" (number->string (+ args-length 2)) "), R1);" nl
        tab "MOV(FPARG(1), " (number->string (+ args-length 1)) ");" nl
        )))))

(define code-gen-lambda-suffix
  (lambda (body lambda-exit)
    (set! enclosed-closures (+ 1 enclosed-closures))
    (let ((generated-code 
           (string-append
            
            "//lambda-body" nl
            (code-gen body) nl
            
            tab "POP(FP);" nl
            tab "RETURN;" nl
            tab "JUMP(" lambda-exit ");" nl
            
            tab lambda-exit ":" nl)))
      (begin (set! enclosed-closures (- enclosed-closures 1))
             generated-code))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;          def          ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-gen-def
  (lambda (pe)
    (with pe
          (lambda (var val)
            (string-append
             "//define " (symbol->string (cadr var)) nl
             (code-gen val) nl
             tab "MOV(IND(" (number->string (cadr (assoc (cadr var) f-table))) "), R0);" nl
             tab "MOV(R0, IMM(SOB_VOID));" nl
             )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;         applic        ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ^label-applic-copy-frame-loop (^^label "L_tc_applic_copy_frame_loop"))
(define ^label-applic-copy-frame-loop-done (^^label "L_tc_applic_copy_frame_loop_done"))

(define code-gen-applic
  (lambda (pe)
    (with pe
          (lambda (f args)
            (string-append
             "//applic" nl
             (code-gen-applic-prefix f args) nl
             tab "CALLA(INDD(R0, 2));" nl
             tab "DROP(1);" nl
             tab "POP(R1);" nl
             tab "DROP(IMM(R1));" nl
             )))))

(define code-gen-tc-applic
  (lambda (pe)
    (with pe
          (lambda (f args)
            (let ((copy-frame-loop (^label-applic-copy-frame-loop)))
              (string-append
               "//tc-applic" nl
               (code-gen-applic-prefix f args) nl
               tab "PUSH(FPARG(-1));" nl
               "//backup the old frame pointer and ret address" nl
               "//R1 <- size of old frame" nl
               tab "MOV(R1, FPARG(1) + 4);" nl
               "//R5 <- old frame start, R6 <- new frame start" nl
               tab "MOV(R5, FP - R1);" nl
               tab "MOV(R6, SP - " (number->string (+ (length args) 3)) ");" nl
               "//replacing current fp with old one" nl
               tab "MOV(FP, FPARG(-2));" nl
               "//R2 <- loop counter" nl
               tab "MOV(R2, " (number->string (+ (length args) 3)) ");" nl
               tab copy-frame-loop ":" nl
               tab "MOV(STACK(R5), STACK(R6))" nl
               tab "INCR(R5);" nl
               tab "INCR(R6);" nl
               tab "DECR(R2);" nl
               tab "CMP(R2, IMM(0));" nl
               tab "JUMP_GT(" copy-frame-loop ");" nl
               "//stack pointer <- end of current frame" nl
               tab "MOV(SP, R5);" nl
               "//jump to the code" nl
               tab "JUMPA(INDD(R0, 2));" nl
               ))))))

(define code-gen-applic-prefix
  (lambda (f args)
    (let ((args-count (number->string (length args)))
          (args-code-gen
           (fold-left (lambda (res sub-pe) (string-append res (code-gen sub-pe) tab "PUSH(R0);" nl))
                      ""
                      (reverse args))))
      (string-append
       "//pushing args" nl
       args-code-gen
       tab "PUSH(IMM(" args-count "));" nl
       (code-gen f) nl
       "//handling errors" nl
       tab "CMP(IND(R0), T_CLOSURE);" nl
       tab "JUMP_NE(ERROR_APPLIC_ON_INVALID_TYPE);" nl
       tab "PUSH(INDD(R0, 1));" nl
       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;          set          ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-gen-set
  (lambda (pe)
    (with pe
          (lambda (var val)
            (string-append
             "//set" nl
             (code-gen val)
             (code-gen-set-var var)
             tab "MOV(R0, IMM(SOB_VOID));" nl
             )))))

(define code-gen-set-var
  (lambda (var)
    (cond ((tagged-by 'bvar var) (with (cdr var)
                                       (lambda (var maj min)
                                         (string-append
                                          tab "MOV(R1, FPARG(0));" nl
                                          tab "MOV(R1, INDD(R1, " (number->string maj) "));" nl
                                          tab "MOV(INDD(R1, " (number->string min) "), R0);" nl))))
          ((tagged-by 'pvar var) (with (cdr var)
                                       (lambda (var min)
                                         (string-append
                                          tab "MOV(FPARG(" (number->string (+ min 2)) "), R0);" nl))))
          ((tagged-by 'fvar var) (with (cdr var)
                                       (lambda (var)
                                         (string-append
                                          tab "MOV(IND(" (number->string (cadr (assoc var f-table))) "), R0);" nl))))
          (else (code-gen-error "Undefined variable type.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;         boxing        ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-gen-box
  (lambda (var)
    (string-append
     "//box " (symbol->string (cadr var)) nl
     (code-gen var)
     tab "PUSH(R0);" nl
     tab "PUSH(1);" nl
     tab "CALL(MALLOC);" nl
     tab "DROP(1);" nl
     tab "POP(R1);" nl
     tab "MOV(IND(R0), R1);" nl
     )))

(define code-gen-box-get
  (lambda (var)
    (string-append
     "//box-get" nl
     (code-gen var)
     tab "MOV(R0,IND(R0));" nl
     )))

(define code-gen-box-set
  (lambda (pe)
    (with pe
          (lambda (var val)
            (string-append
             "//box-set" nl
             (code-gen val)
             (code-gen-box-set-var var)
             tab "MOV(R0, IMM(SOB_VOID));" nl
             )))))

(define code-gen-box-set-var
  (lambda (var)
    (cond ((tagged-by 'bvar var) (with (cdr var)
                                       (lambda (var maj min)
                                         (string-append
                                          tab "MOV(R1, FPARG(0));" nl
                                          tab "MOV(R1, INDD(R1, " (number->string maj) "));" nl
                                          tab "MOV(R1, INDD(R1, " (number->string min) "));" nl
                                          tab "MOV(IND(R1), R0)" nl))))
          ((tagged-by 'pvar var) (with (cdr var)
                                       (lambda (var min)
                                         (string-append
                                          tab "MOV(R1, FPARG(" (number->string (+ min 2)) "));" nl
                                          tab "MOV(IND(R1), R0);" nl
                                          ))))
          ((tagged-by 'fvar var) (with (cdr var)
                                       (lambda (var)
                                         (string-append
                                          tab "MOV(R1, IND(" (number->string (cadr (assoc (cadr var) f-table))) "));" nl
                                          tab "MOV(IND(R1), R0);" nl
                                          ))))
          (else (code-gen-error "Undefined variable type.")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;        c-table        ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ^c-table
  (lambda (pe)
    (c-table-add `((100 ,*void-object* (,T_VOID)) (101 () (,T_NIL)) (102 #f (,T_BOOL 0)) (104 #t (,T_BOOL 1)))
                 (extract-constants pe)
                 106)))

(define topological-sort 
  (lambda (e) 
    (cond 
      ((or (number? e) (string? e) (eq? e (void)) (null? e) (boolean? e) (char? e) ) `(,e)) 
      ((pair? e) 
       `(,e ,@(topological-sort (car e)) ,@(topological-sort (cdr e))))
      ((vector? e) 
       `(,e ,@(apply append 
                     (map topological-sort (vector->list e)))))
      ((symbol? e)
       `(,e ,@(topological-sort (symbol->string e))))
      (else 'topological-sort-error))))

(define extract-constants
  (lambda (pe)
    (cond ((atom? pe) '())
          ((tagged-by 'const pe) (reverse (topological-sort (cadr pe))))
          (else `(,@(extract-constants (car pe)) ,@(extract-constants (cdr pe)))))))

(define c-table-contains?
  (lambda (table const)
    (cond ((null? table) #f)
          ((equal? const (cadar table)) (caar table))
          (else (c-table-contains? (cdr table) const)))))

(define get-address c-table-contains?)

(define c-table-add
  (lambda (table lst mem-index)
    (cond ((null? lst)
           table)
          ((c-table-contains? table (car lst))
           `(,@(c-table-add table (cdr lst) mem-index)))
          ((char? (car lst))
           (c-table-add `(,@table (,mem-index ,(car lst) (,T_CHAR ,(char->integer (car lst)))))
                        (cdr lst)
                        (+ mem-index 2)))
          ((integer? (car lst))
           (c-table-add `(,@table (,mem-index ,(car lst) (,T_INTEGER ,(car lst))))
                        (cdr lst)
                        (+ mem-index 2)))
          ((rational? (car lst))
           (c-table-add `(,@table (,mem-index ,(car lst) (,T_FRACTION ,(numerator (car lst)) ,(denominator (car lst)))))
                        (cdr lst)
                        (+ mem-index 3)))
          ((string? (car lst))
           (c-table-add `(,@table (,mem-index ,(car lst) (,T_STRING ,(string-length (car lst)) ,(map char->integer (string->list (car lst))))))
                        (cdr lst)
                        (+ mem-index (string-length (car lst)) 2)))
          ((symbol? (car lst))
           (let ((rep-str (symbol->string (car lst))))
             (if (c-table-contains? table rep-str)
                 (c-table-add `(,@table (,mem-index ,(car lst) (,T_SYMBOL ,(get-address table rep-str))))
                              (cdr lst)
                              (+ mem-index 2))
                 (c-table-add `(,@table (,mem-index ,rep-str (,T_STRING ,(string-length rep-str) ,(map char->integer (string->list rep-str))))
                                        (,(+ mem-index (string-length rep-str) 2) ,(car lst) (,T_SYMBOL ,mem-index)))
                              (cdr lst)
                              (+ mem-index (string-length rep-str) 4)))))
          ((pair? (car lst))
           (c-table-add `(,@table (,mem-index ,(car lst) (,T_PAIR ,(get-address table (caar lst)) ,(get-address table (cdar lst)))))
                        (cdr lst)
                        (+ mem-index 3)))
          ((vector? (car lst))
           (c-table-add `(,@table (,mem-index ,(car lst) (,T_VECTOR ,(vector-length (car lst)) ,(map (lambda (item) (get-address table item)) (vector->list (car lst))))))
                        (cdr lst)
                        (+ mem-index (vector-length (car lst)) 2)))
          (else 'error-c-table-add))))

(define get-line-data
  (lambda (line)
    (caddr line)))

(define type-of?
  (lambda (type line)
    (equal? type (car (get-line-data line)))))

(define code-gen-c-table
  (lambda (table)
    (fold-left string-append
               "// c-table initialization\n"
               (map (lambda (line)
                      (let ((data (get-line-data line)))
                        (string-append
                         (cond ((type-of? T_VOID line) (code-gen-void))
                               ((type-of? T_NIL line) (code-gen-nil))
                               ((type-of? T_BOOL line) (code-gen-boolean (cadr data)))
                               ((type-of? T_CHAR line) (code-gen-char (cadr data)))
                               ((type-of? T_INTEGER line) (code-gen-integer (cadr data)))
                               ((type-of? T_FRACTION line) (code-gen-fraction (cadr data) (caddr data)))
                               ((type-of? T_STRING line) (code-gen-string (cadr data) (caddr data)))
                               ((type-of? T_SYMBOL line) (code-gen-symbol (cadr data)))
                               ((type-of? T_PAIR line) (code-gen-pair (cadr data) (caddr data)))
                               ((type-of? T_VECTOR line) (code-gen-vector (cadr data) (caddr data)))
                               (else 'undefined_type)))))
                    table))))

(define code-gen-void
  (lambda ()
    (string-append
     tab "CALL(MAKE_SOB_VOID);" nl)))

(define code-gen-nil
  (lambda ()
    (string-append
     tab "CALL(MAKE_SOB_NIL);" nl)))

(define code-gen-boolean
  (lambda (value)
    (string-append
     tab "PUSH(IMM(" (number->string value) "));" nl
     tab "CALL(MAKE_SOB_BOOL);" nl
     tab "DROP(1);" nl)))

(define code-gen-char
  (lambda (value)
    (string-append
     tab "PUSH(IMM(" (number->string value) "));" nl
     tab "CALL(MAKE_SOB_CHAR);" nl
     tab "DROP(1);" nl)))

(define code-gen-integer
  (lambda (value)
    (string-append
     tab "PUSH(IMM(" (number->string value) "));" nl
     tab "CALL(MAKE_SOB_INTEGER);" nl
     tab "DROP(1);" nl)))

(define code-gen-fraction
  (lambda (num denum)
    (string-append
     tab "PUSH(IMM(" (number->string denum) "));" nl
     tab "PUSH(IMM(" (number->string num) "));" nl
     tab "CALL(MAKE_SOB_FRACTION);" nl
     tab "DROP(2);" nl)))

(define code-gen-string
  (lambda (length chars)
    (let ((chars (fold-left (lambda (acc current)
                              (string-append acc tab "PUSH(IMM(" (number->string current) "));" nl))
                            ""
                            chars)))
      (string-append
       chars
       tab "PUSH(IMM(" (number->string length) "));" nl
       tab "CALL(MAKE_SOB_STRING);" nl
       tab "POP(R1);" nl
       tab "DROP(R1);" nl))))

(define code-gen-symbol
  (lambda (str-add)
    (string-append
     tab "PUSH(IMM(" (number->string str-add) "));" nl
     tab "CALL(MAKE_SOB_SYMBOL);" nl
     tab "DROP(1);" nl)))

(define code-gen-pair
  (lambda (car cdr)
    (string-append
     tab "PUSH(IMM(" (number->string cdr) "));" nl
     tab "PUSH(IMM(" (number->string car) "));" nl
     tab "CALL(MAKE_SOB_PAIR);" nl
     tab "DROP(2);" nl)))

(define code-gen-vector
  (lambda (length items)
    (let ((items (fold-left (lambda (acc current)
                              (string-append acc tab "PUSH(IMM(" (number->string current) "));" nl))
                            ""
                            items)))
      (string-append
       items
       tab "PUSH(IMM(" (number->string length) "));" nl
       tab "CALL(MAKE_SOB_VECTOR);" nl
       tab "POP(R1);" nl
       tab "DROP(IND(R1));" nl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;        f-table        ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ^f-table
  (lambda (pe mem-add)
    (f-table-add (^primitives-table '() primitives mem-add)
                 (extract-fvars pe)
                 (+ (length primitives) mem-add))))

(define ^primitives-table
  (lambda (table primitives mem-add)
    (if (null? primitives) table
        (^primitives-table `(,@table (,(car primitives) ,mem-add))
                           (cdr primitives)
                           (+ mem-add 1)))))

(define extract-fvars
  (lambda (pe)
    (cond ((atom? pe) '())
          ((tagged-by 'fvar pe) (cdr pe))
          (else `(,@(extract-fvars (car pe)) ,@(extract-fvars (cdr pe)))))))

(define f-table-contains?
  (lambda (table fvar)
    (cond ((null? table) #f)
          ((equal? fvar (caar table)) (cadar table))
          (else (f-table-contains? (cdr table) fvar)))))

(define f-table-add
  (lambda (table lst mem-index)
    (cond ((null? lst)
           table)
          ((f-table-contains? table (car lst))
           `(,@(f-table-add table (cdr lst) mem-index)))
          (else (f-table-add `(,@table (,(car lst) ,mem-index)) (cdr lst) (+ mem-index 1)))
          )))

(define find-current-mem-index
  (lambda ()
    (let ((lp (car (last-pair c-table))))
      (+ (car lp) (length (flatten (caddr lp)))))))

(define code-gen-f-table
  (lambda (table)
    (string-append
     tab "PUSH(" (number->string (length table)) ");" nl
     tab "CALL(MALLOC);" nl
     tab "DROP(1);" nl
     )
    
    #;(fold-left string-append
                 "// f-table initialization"
                 (map (lambda (line)
                        (string-append
                         tab "PUSH(1);" nl
                         tab "CALL(MALLOC);" nl
                         tab "DROP(1);" nl))
                      table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;        sym-list       ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define remove-last-pair
  (lambda (lst)
    (if (or (null? lst) (null? (cdr lst)))
        '()
        (cons (car lst) (remove-last-pair (cdr lst))))))


(define code-gen-sym-list
  (lambda (c-table)
    (let* ((symbols (filter (lambda (line)
                              (type-of? T_SYMBOL line))
                            c-table))
           (addresses (map (lambda (line) (cadr (caddr line))) symbols))
           (code-gen-list (if (null? addresses)
                              (string-append
                               tab "MOV(R2, SYMBOLS_LIST);" nl
                               tab "MOV(IND(R2), SOB_NIL);" nl)
                              (string-append
                               (fold-left
                                string-append
                                (string-append
                                 tab "PUSH(2);" nl
                                 tab "CALL(MALLOC);" nl
                                 tab "DROP(1);" nl
                                 tab "MOV(R2, SYMBOLS_LIST);" nl
                                 tab "MOV(IND(R2), R0);" nl
                                 )
                                (map (lambda (address)
                                       (string-append
                                        tab "MOV(R1, R0);" nl
                                        tab "PUSH(2);" nl
                                        tab "CALL(MALLOC);" nl
                                        tab "DROP(1);" nl
                                        tab "MOV(IND(R1), " (number->string address) ");" nl
                                        tab "MOV(INDD(R1, 1), R0);" nl
                                        ))
                                     (remove-last-pair addresses)))
                               tab "MOV(IND(R0), " (number->string (car (last-pair addresses))) ");" nl
                               tab "MOV(INDD(R0, 1), SOB_NIL);" nl
                               
                               ))))
      (string-append
       "// symbols list:" nl
       code-gen-list
       ))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;         prolog        ;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (load "src/primitives.scm")
    
    (define prolog
      (string-append
       "#include <stdio.h>" nl
       "#include <stdlib.h>" nl
       "#include \"arch/cisc.h\"" nl
       
       "/* change to 0 for no debug info to be printed: */" nl
       "#define DO_SHOW 1" nl
       
       "#define SYMBOLS_LIST 99" nl
       "#define SOB_VOID 100" nl
       "#define SOB_NIL 101" nl
       "#define SOB_FALSE 102" nl
       "#define SOB_TRUE 104" nl
       
       "int main(){" nl
       tab "START_MACHINE;" nl
       
       "/* mem-address start from 100: */" nl
       tab "MOV(IND(0), IMM(100));" nl
       
       tab "JUMP(CONTINUE);" nl
       tab "#include \"arch/char.lib\"" nl
       tab "#include \"arch/io.lib\"" nl
       tab "#include \"arch/math.lib\"" nl
       tab "#include \"arch/string.lib\"" nl
       tab "#include \"arch/system.lib\"" nl
       tab "#include \"arch/scheme.lib\"" nl
       
       (code-gen-primitives)
       
       tab "ERROR:" nl
       tab "PUSH(R0);" nl
       tab "CALL(WRITE_SOB);" nl
       tab "DROP(1);" nl
       tab "JUMP(END);" nl
       
       tab "CONTINUE:" nl
       ))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;         errors        ;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define code-gen-error
      (lambda (str)
        (let ((chars (fold-left (lambda (acc current)
                                  (string-append acc tab "PUSH(IMM(" (number->string (char->integer current)) "));" nl))
                                ""
                                (string->list str))))
          (string-append
           chars
           tab "PUSH(IMM(" (number->string (string-length str)) "));" nl
           tab "CALL(MAKE_SOB_STRING);" nl
           tab "POP(R1);" nl
           tab "DROP(IND(R1));" nl
           tab "JUMP(ERROR);" nl))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;        epilogue       ;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define epilogue
      (string-append
       nl "// epilogue" nl
       tab "END:" nl
       
       tab "STOP_MACHINE;" nl
       tab "return 0;" nl
       
       tab "ERROR_UNDEFINED_FVAR:"
       (code-gen-error "Free variable undefined")
       tab "ERROR_LAMBDA_INVALID_ARGS_COUNT:"
       (code-gen-error "Error: Invalid number of arguments.")
       tab "ERROR_APPLIC_ON_INVALID_TYPE:"
       (code-gen-error "Error: Application on non-closure.")
       
       "}" nl
       ))