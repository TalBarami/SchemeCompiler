;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;      primitives       ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define PUSHAD
  (string-append
   tab "PUSH(R1); PUSH(R2); PUSH(R3); PUSH(R4); PUSH(R5); PUSH(R6); PUSH(R7); PUSH(R8); PUSH(R9); PUSH(R10);" nl
   ))

(define POPAD
  (string-append
   tab "POP(R10); POP(R9); POP(R8); POP(R7); POP(R6); POP(R5); POP(R4); POP(R3); POP(R2); POP(R1);" nl
   ))

(define code-gen-type?
  (lambda (label . expected)
    (let ((expected (fold-left string-append
                               ""
                               (map (lambda (type)
                                      (string-append
                                       tab "CMP(IND(R0), IMM(" type "));" nl
                                       tab "JUMP_EQ(" label "_true);" nl))
                                    expected))))
      (string-append
       "L_" label "_pred:" nl
       tab "PUSH(FP);" nl
       tab "MOV(FP, SP);" nl
       PUSHAD
       tab "MOV(R0, FPARG(2));" nl
       expected
       tab "MOV(R0, IMM(SOB_FALSE));" nl
       tab "JUMP(" label "_exit);" nl
       tab label "_true:" nl
       tab "MOV(R0, IMM(SOB_TRUE));" nl
       tab label "_exit:" nl
       POPAD
       tab "POP(FP);" nl
       tab "RETURN;" nl))))

(define code-gen-null?
  (code-gen-type? "null" "T_NIL"))

(define code-gen-boolean?
  (code-gen-type? "boolean" "T_BOOL"))

(define code-gen-char?
  (code-gen-type? "char" "T_CHAR"))

(define code-gen-integer?
  (code-gen-type? "integer" "T_INTEGER"))

(define code-gen-rational?
  (code-gen-type? "rational" "T_INTEGER" "T_FRACTION"))

(define code-gen-number?
  (code-gen-type? "number" "T_INTEGER" "T_FRACTION"))

(define code-gen-pair?
  (code-gen-type? "pair" "T_PAIR"))

(define code-gen-string?
  (code-gen-type? "string" "T_STRING"))

(define code-gen-symbol?
  (code-gen-type? "symbol" "T_SYMBOL"))

(define code-gen-vector?
  (code-gen-type? "vector" "T_VECTOR"))

(define code-gen-procedure?
  (code-gen-type? "procedure" "T_CLOSURE"))

(define code-gen-cons
  (string-append
   "L_cons:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "PUSH(FPARG(3));" nl
   tab "PUSH(FPARG(2));" nl
   tab "CALL(MAKE_SOB_PAIR);" nl
   tab "DROP(2);" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-car
  (string-append
   "L_car:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "MOV(R0, INDD(FPARG(2), 1));" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-cdr
  (string-append
   "L_cdr:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "MOV(R0, INDD(FPARG(2), 2));" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-set-car
  (string-append
   "L_set_car:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "MOV(R0, FPARG(2));" nl
   tab "MOV(R1, FPARG(3));" nl
   tab "MOV(INDD(R0, 1), R1);" nl
   tab "MOV(R0, IMM(SOB_VOID)); " nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-set-cdr
  (string-append
   "L_set_cdr:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "MOV(R0, FPARG(2));" nl
   tab "MOV(R1, FPARG(3));" nl
   tab "MOV(INDD(R0, 2), R1);" nl
   tab "MOV(R0, IMM(SOB_VOID)); " nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-apply
  (string-append
   "L_apply:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "MOV(R1, IMM(0)); " nl
   tab "MOV(R2, FPARG(3));" nl
   tab "L_apply_loop:" nl
   tab "CMP(IND(R2), T_NIL);" nl
   tab "JUMP_EQ(L_apply_loop_exit);" nl
   tab "PUSH(INDD(R2, 1));" nl
   tab "MOV(R2, INDD(R2, 2));" nl
   tab "INCR(R1);" nl
   tab "JUMP(L_apply_loop);" nl
   tab "L_apply_loop_exit:" nl
   tab "MOV(R4, SP);" nl
   tab "SUB(R4, R1);" nl
   tab "MOV(R3, SP);" nl
   tab "DECR(R3);" nl
   tab "L_apply_reverse_loop:" nl
   tab "CMP(R4, R3);" nl
   tab "JUMP_GE(L_apply_reverse_exit);" nl
   tab "PUSH(R1);" nl
   tab "MOV(R1, STACK(R4));" nl
   tab "MOV(STACK(R4), STACK(R3));" nl
   tab "MOV(STACK(R3), R1);" nl
   tab "POP(R1);" nl
   tab "INCR(R4);" nl
   tab "DECR(R3);" nl
   tab "JUMP(L_apply_reverse_loop);" nl
   tab "L_apply_reverse_exit:" nl
   tab "PUSH(R1);" nl
   tab "MOV(R4, FPARG(2));" nl
   tab "PUSH(INDD(R4, 1));" nl
   tab "CALLA(INDD(R4, 2));" nl
   tab "MOV(R2, STARG(0)); " nl
   tab "ADD(R2, IMM(2));" nl
   tab "SUB(SP, R2);" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-numerator
  (string-append
   "L_numerator:"
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "PUSH(INDD(FPARG(2), 1));" nl
   tab "CALL(MAKE_SOB_INTEGER);" nl
   tab "DROP(1);" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-denominator
  (string-append
   "L_denominator:"
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "CMP(IND(FPARG(2)), IMM(T_INTEGER));" nl
   tab "JUMP_EQ(L_denominator_is_1);" nl
   tab "PUSH(INDD(FPARG(2), 2));" nl
   tab "JUMP(L_denominator_return);" nl
   tab "L_denominator_is_1:" nl
   tab "PUSH(IMM(1));" nl
   tab "L_denominator_return:" nl
   tab "CALL(MAKE_SOB_INTEGER);" nl
   tab "DROP(1);" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-get-bin-op-args
  (string-append
   tab "//R1 <- ARG[0] numerator" nl
   tab "PUSH(FPARG(2));" nl
   tab "PUSH(1);" nl
   tab "PUSH(1);" nl
   tab "CALL(L_numerator);" nl
   tab "DROP(3);" nl
   tab "MOV(R1, INDD(R0,1));" nl
   
   tab "//R2 <- ARG[0] denominator" nl
   tab "PUSH(FPARG(2));" nl
   tab "PUSH(1);" nl
   tab "PUSH(1);" nl
   tab "CALL(L_denominator);" nl
   tab "DROP(3);" nl
   tab "MOV(R2, INDD(R0,1));" nl
   
   tab "//R3 <- ARG[1] numerator" nl
   tab "PUSH(FPARG(3));" nl
   tab "PUSH(1);" nl
   tab "PUSH(1);" nl
   tab "CALL(L_numerator);" nl
   tab "DROP(3);" nl
   tab "MOV(R3, INDD(R0,1));" nl
   
   tab "//R4 <- ARG[1] denominator" nl
   tab "PUSH(FPARG(3));" nl
   tab "PUSH(1);" nl
   tab "PUSH(1);" nl
   tab "CALL(L_denominator);" nl
   tab "DROP(3);" nl
   tab "MOV(R4, INDD(R0,1));" nl
   ))

(define code-gen-bin-result
  (lambda (op)
    (string-append
     tab "PUSH(R2);" nl
     tab "PUSH(R1);" nl
     tab "CALL(GCD);" nl
     tab "POP(R1);" nl
     tab "POP(R2);" nl
     tab "DIV(R1, R0);" nl
     tab "DIV(R2, R0);" nl
     
     tab "CMP(R2, 1);" nl
     tab "JUMP_EQ(L_bin_" op "_return_numerator);" nl
     tab "CMP(R1, 0);" nl
     tab "JUMP_EQ(L_bin_" op "_return_numerator);" nl
     tab "CMP(R1, R2);" nl
     tab "JUMP_EQ(L_bin_" op "_return_1);" nl
     
     tab "PUSH(R2);" nl
     tab "PUSH(R1);" nl
     tab "CALL(MAKE_SOB_FRACTION);" nl
     tab "DROP(1);" nl
     tab "JUMP(L_bin_" op "_return);" nl
     
     tab "L_bin_" op "_return_numerator:" nl
     tab "PUSH(R1);" nl
     tab "CALL(MAKE_SOB_INTEGER);" nl
     tab "JUMP(L_bin_" op "_return);" nl
     
     tab "L_bin_" op "_return_1:" nl
     tab "PUSH(IMM(1));" nl
     tab "CALL(MAKE_SOB_INTEGER);" nl
     
     tab "L_bin_" op "_return:" nl
     tab "DROP(1);" nl
     )))

(define code-gen-bin-add
  (string-append
   "L_ADD_bin:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   code-gen-get-bin-op-args
   tab "MUL(R1,R4)" nl
   tab "MUL(R3,R2)" nl
   tab "MUL(R2,R4)" nl
   tab "ADD(R1,R3)" nl
   (code-gen-bin-result "ADD")
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))


(define code-gen-bin-sub
  (string-append
   "L_SUB_bin:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   code-gen-get-bin-op-args
   tab "MUL(R1,R4)" nl
   tab "MUL(R3,R2)" nl
   tab "MUL(R2,R4)" nl
   tab "SUB(R1,R3)" nl
   (code-gen-bin-result "SUB")
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-bin-mul
  (string-append
   "L_MUL_bin:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   code-gen-get-bin-op-args
   tab "MUL(R1,R3)" nl
   tab "MUL(R2,R4)" nl
   (code-gen-bin-result "MUL")
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-bin-div
  (string-append
   "L_DIV_bin:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   code-gen-get-bin-op-args
   tab "MUL(R1,R4)" nl
   tab "MUL(R2,R3)" nl
   (code-gen-bin-result "DIV")
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-bin<?
  (string-append
   "L_less_than:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "PUSH(FPARG(3));" nl
   tab "PUSH(FPARG(2));" nl
   tab "PUSH(2);" nl
   tab "PUSH(1);" nl
   tab "CALL(L_SUB_bin);" nl
   tab "DROP(4);" nl
   tab "MOV(R0, INDD(R0, 1));" nl
   tab "CMP(R0, 0);" nl
   tab "JUMP_LT(L_less_than_true);" nl
   tab "MOV(R0, IMM(SOB_FALSE));" nl
   tab "JUMP(L_less_than_return);" nl
   tab "L_less_than_true:" nl
   tab "MOV(R0, IMM(SOB_TRUE));" nl
   tab "L_less_than_return:" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-bin=?
  (string-append
   "L_equal:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "PUSH(FPARG(3));" nl
   tab "PUSH(FPARG(2));" nl
   tab "PUSH(2);" nl
   tab "PUSH(1);" nl
   tab "CALL(L_SUB_bin);" nl
   tab "DROP(4);" nl
   tab "MOV(R0, INDD(R0, 1));" nl
   tab "CMP(R0, 0);" nl
   tab "JUMP_EQ(L_equal_true);" nl
   tab "MOV(R0, IMM(SOB_FALSE));" nl
   tab "JUMP(L_equal_return);" nl
   tab "L_equal_true:" nl
   tab "MOV(R0, IMM(SOB_TRUE));" nl
   tab "L_equal_return:" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-eq?
  (string-append
   "L_eq:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "CMP(IND(FPARG(2)), IND(FPARG(3)));" nl
   tab "JUMP_NE(L_eq_return_false);" nl
   tab "CMP(IND(FPARG(2)), T_INTEGER);" nl
   tab "JUMP_EQ(L_eq_compare_by_value);" nl
   tab "CMP(IND(FPARG(2)), T_FRACTION);" nl
   tab "JUMP_EQ(L_eq_compare_fraction_by_value);" nl
   tab "CMP(IND(FPARG(2)), T_CHAR);" nl
   tab "JUMP_EQ(L_eq_compare_by_value);" nl
   tab "CMP(IND(FPARG(2)), T_SYMBOL);" nl
   tab "JUMP_EQ(L_eq_compare_by_value);" nl
   tab "JUMP(L_eq_compare_by_address);" nl
   tab "L_eq_compare_fraction_by_value:" nl
   tab "CMP(INDD(FPARG(2), 2), INDD(FPARG(3), 2));" nl
   tab "JUMP_NE(L_eq_return_false);" nl
   tab "L_eq_compare_by_value:" nl
   tab "CMP(INDD(FPARG(2), 1), INDD(FPARG(3), 1));" nl
   tab "JUMP_NE(L_eq_return_false);" nl
   tab "JUMP(L_eq_return_true);" nl
   tab "L_eq_compare_by_address:" nl
   tab "CMP(FPARG(2), FPARG(3));" nl
   tab "JUMP_NE(L_eq_return_false);" nl
   tab "JUMP(L_eq_return_true);" nl
   tab "L_eq_return_false:" nl
   tab "MOV(R0, IMM(SOB_FALSE));" nl
   tab "JUMP(L_eq_return);" nl
   tab "L_eq_return_true:" nl
   tab "MOV(R0, IMM(SOB_TRUE));" nl
   tab "L_eq_return:" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-remainder
  (string-append
   "L_remainder:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "MOV(R0,INDD(FPARG(2),1));" nl
   tab "MOV(R1,INDD(FPARG(3),1));" nl
   tab "REM(R0, R1);" nl
   tab "PUSH(R0);" nl
   tab "CALL(MAKE_SOB_INTEGER);" nl
   tab "DROP(1);" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-char->integer
  (string-append
   "L_char_to_integer:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "MOV(R0, INDD(FPARG(2), 1));" nl
   tab "PUSH(R0);" nl
   tab "CALL(MAKE_SOB_INTEGER);" nl
   tab "DROP(1);" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-integer->char
  (string-append
   "L_integer_to_char:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "MOV(R0, INDD(FPARG(2), 1));" nl
   tab "PUSH(R0);" nl
   tab "CALL(MAKE_SOB_CHAR);" nl
   tab "DROP(1);" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-make-string
  (string-append
   "L_make_string:"
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "MOV(R1, INDD(FPARG(2), 1));" nl
   tab "MOV(R3, R1);" nl
   tab "CMP(FPARG(1), 1);" nl
   tab "JUMP_EQ(L_make_string_one_argument);" nl
   tab "MOV(R2, FPARG(3));" nl
   tab "JUMP(L_make_string_loop);" nl
   tab "L_make_string_one_argument:" nl
   tab "PUSH(0);" nl
   tab "CALL(MAKE_SOB_CHAR);" nl
   tab "DROP(1);" nl
   tab "MOV(R2, R0);" nl
   tab "L_make_string_loop:" nl
   tab "CMP(R1,IMM(0));" nl
   tab "JUMP_EQ(L_make_string_loop_end);" nl
   tab "PUSH(INDD(R2,1));" nl
   tab "DECR(R1);" nl
   tab "JUMP(L_make_string_loop);" nl
   tab "L_make_string_loop_end:" nl
   tab "PUSH(R3);" nl
   tab "CALL(MAKE_SOB_STRING);" nl
   tab "POP(R3);" nl
   tab "DROP(R3);" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-string-set!
  (string-append
   "L_string_set:"
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "MOV(R0, INDD(FPARG(3), 1));" nl
   tab "ADD(R0, 2);" nl
   tab "MOV(R1, INDD(FPARG(4), 1));" nl
   tab "MOV(INDD(FPARG(2), R0), R1);" nl
   tab "MOV(R0, SOB_VOID);" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-string-length
  (string-append
   "L_string_length:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "MOV(R0, (INDD(FPARG(2),1)));" nl
   tab "PUSH(R0);" nl
   tab "CALL(MAKE_SOB_INTEGER);" nl
   tab "DROP(1);" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-string-ref
  (string-append
   "L_string_ref:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "MOV(R1, INDD(FPARG(3), 1));" nl
   tab "ADD(R1, 2);" nl
   tab "MOV(R0, FPARG(2));" nl
   tab "PUSH(INDD(R0, R1));" nl
   tab "CALL(MAKE_SOB_CHAR);" nl
   tab "DROP(1);" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-make-vector
  (string-append
   "L_make_vector:"
   tab "PUSH(FP);" nl
   tab "MOV(FP,SP);" nl
   PUSHAD
   tab "CMP(FPARG(1),IMM(1));" nl
   tab "JUMP_EQ(L_make_vector_one_argument);" nl
   tab "MOV(R2,FPARG(3));" nl
   tab "JUMP(L_make_vector_continue);" nl
   "L_make_vector_one_argument:"
   tab "PUSH(0);" nl
   tab "CALL(MAKE_SOB_INTEGER);" nl
   tab "DROP(1);" nl
   tab "MOV(R2, R0);" nl
   "L_make_vector_continue:"
   tab "MOV(R3, INDD(FPARG(2), 1));" nl
   tab "MOV(R4,R3);" nl
   "L_make_vector_loop:"
   tab "CMP(R3, IMM(0));" nl
   tab "JUMP_EQ(L_make_vector_loop_end);" nl
   tab "PUSH(IMM(R2));" nl
   tab "DECR(R3);" nl
   tab "JUMP(L_make_vector_loop);" nl
   "L_make_vector_loop_end:"
   tab "PUSH(R4);" nl
   tab "CALL(MAKE_SOB_VECTOR);" nl
   tab "POP(R1);" nl
   tab "DROP(R1);" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-vector-set!
  (string-append
   "L_vector_set:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "MOV(R0,INDD(FPARG(3), 1));" nl
   tab "ADD(R0, IMM(2));" nl
   tab "MOV(R1, FPARG(4));" nl
   tab "MOV(INDD(FPARG(2), R0), R1);" nl
   tab "MOV(R0, SOB_VOID);" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-vector-length
  (string-append
   "L_vector_length:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "MOV(R0, (INDD(FPARG(2), 1)));" nl
   tab "PUSH(R0);" nl
   tab "CALL(MAKE_SOB_INTEGER);" nl
   tab "DROP(1);" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-vector-ref
  (string-append
   "L_vector_ref:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "MOV(R1, INDD(FPARG(3), 1));" nl
   tab "ADD(R1, 2);" nl
   tab "MOV(R0, FPARG(2));" nl
   tab "MOV(R0, INDD(R0, R1));" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-string-equals?
  (string-append
   "L_string_equals:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   PUSHAD
   tab "MOV(R1, FPARG(2));" nl
   tab "MOV(R2, FPARG(3));" nl
   tab "MOV(R3, INDD(R1, 1));" nl
   tab "MOV(R4, INDD(R2, 1));" nl
   tab "CMP(R3, R4);" nl
   tab "JUMP_NE(L_string_equals_return_false);" nl
   tab "MOV(R4, 0);" nl
   tab "L_string_equals_loop_start:" nl
   tab "MOV(R5, INDD(R1, R4 + 2));" nl
   tab "MOV(R6, INDD(R2, R4 + 2));" nl
   tab "CMP(R5, R6);" nl
   tab "JUMP_NE(L_string_equals_return_false);" nl
   tab "ADD(R4, 1);" nl
   tab "CMP(R4, R3);" nl
   tab "JUMP_NE(L_string_equals_loop_start);" nl
   tab "MOV(R0, SOB_TRUE);" nl
   tab "JUMP(L_string_equals_return);" nl
   tab "L_string_equals_return_false:" nl
   tab "MOV(R0, SOB_FALSE);" nl
   tab "L_string_equals_return:" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))

(define code-gen-symbol->string
  (string-append
   "L_symbol_to_string:" nl
   tab "PUSH(FP);" nl
   tab "MOV(FP, SP);" nl
   tab "MOV(R0, INDD(FPARG(2), 1));" nl
   tab "POP(FP);" nl
   tab "RETURN;" nl
   ))
   
(define code-gen-string->symbol
  (string-append
   "L_string_to_symbol:"
   tab "PUSH(FP);" nl
   tab "MOV(FP,SP);" nl
   PUSHAD
   tab "MOV(R1, FPARG(2));" nl
   tab "MOV(R2, SYMBOLS_LIST);" nl
   tab "CMP(IND(R2), SOB_NIL);" nl
   tab "JUMP_NE(L_string_to_symbol_loop_start);" nl
   tab "// If list is empty - initialize new one:" nl
   tab "PUSH(2);" nl
   tab "CALL(MALLOC);" nl
   tab "DROP(1);" nl
   tab "MOV(IND(R0), R1);" nl
   tab "MOV(INDD(R0, 1), SOB_NIL);" nl
   tab "MOV(IND(R2), R0);" nl
   tab "MOV(R2, IND(R2));" nl
   tab "JUMP(L_string_to_symbol_create_new_symbol);" nl
   
   tab "L_string_to_symbol_loop_start:" nl
   tab "MOV(R2, IND(R2));" nl
   tab "PUSH(IND(R2));" nl
   tab "PUSH(R1);" nl
   tab "PUSH(2);" nl
   tab "PUSH(1);" nl
   tab "CALL(L_string_equals);" nl
   tab "DROP(4);" nl
   tab "CMP(R0, SOB_TRUE);" nl
   tab "JUMP_EQ(L_string_to_symbol_create_new_symbol);" nl
   tab "MOV(R3, R2);" nl
   tab "MOV(R2, INDD(R2, 1));" nl
   tab "CMP(R2, SOB_NIL);" nl
   tab "JUMP_NE(L_string_to_symbol_loop_start);" nl
   tab "PUSH(2);" nl
   tab "CALL(MALLOC);" nl
   tab "DROP(1);" nl
   tab "MOV(IND(R0), R1);" nl
   tab "MOV(INDD(R0, 1), SOB_NIL);" nl
   tab "MOV(INDD(R3, 1), R0);" nl
   tab "MOV(R2, INDD(R3, 1));" nl
   
   tab "L_string_to_symbol_create_new_symbol:" nl
   tab "PUSH(IND(R2));" nl
   tab "CALL(MAKE_SOB_SYMBOL);" nl
   tab "DROP(1);" nl
   POPAD
   tab "POP(FP);" nl
   tab "RETURN;" nl
   )
)

(define code-gen-primitives
  (lambda ()
    (string-append
     code-gen-null?
     code-gen-boolean?
     code-gen-char?
     code-gen-integer?
     code-gen-rational?
     code-gen-number?
     code-gen-pair?
     code-gen-string?
     code-gen-symbol?
     code-gen-vector?
     code-gen-procedure?
     
     code-gen-cons
     code-gen-car
     code-gen-cdr
     code-gen-set-car
     code-gen-set-cdr
     
     code-gen-apply
     
     code-gen-bin-add
     code-gen-bin-sub
     code-gen-bin-mul
     code-gen-bin-div
     code-gen-numerator
     code-gen-denominator
     code-gen-bin<?
     code-gen-bin=?
     code-gen-eq?
     code-gen-remainder
     
     code-gen-char->integer
     code-gen-integer->char
     
     code-gen-make-string
     code-gen-string-set!
     code-gen-string-length
     code-gen-string-ref
     
     code-gen-make-vector
     code-gen-vector-set!
     code-gen-vector-length
     code-gen-vector-ref
     
     code-gen-string-equals?
     code-gen-symbol->string
     code-gen-string->symbol
     )))

(define asm-primitive?
  (lambda (x)
    (member x primitives)))

(define primitives-labels
  '((null? "L_null_pred") (boolean? "L_boolean_pred") (char? "L_char_pred") (integer? "L_integer_pred") (rational? "L_rational_pred") (number? "L_number_pred")
                          (pair? "L_pair_pred") (string? "L_string_pred") (symbol? "L_symbol_pred") (vector? "L_vector_pred") (procedure? "L_procedure_pred")
                          (cons "L_cons") (car "L_car") (cdr "L_cdr") (set-car! "L_set_car") (set-cdr! "L_set_cdr") (apply "L_apply")
                          (bin+ "L_ADD_bin") (bin- "L_SUB_bin") (bin* "L_MUL_bin") (bin/ "L_DIV_bin") (numerator "L_numerator") (denominator "L_denominator")
                          (bin<? "L_less_than") (bin=? "L_equal") (eq? "L_eq") (remainder "L_remainder") (char->integer "L_char_to_integer") (integer->char "L_integer_to_char")
                          (make-string "L_make_string") (string-set! "L_string_set") (string-length "L_string_length") (string-ref "L_string_ref")
                          (make-vector "L_make_vector") (vector-set! "L_vector_set") (vector-length "L_vector_length") (vector-ref "L_vector_ref")
                          (string-equals? "L_string_equals") (symbol->string "L_symbol_to_string") (string->symbol "L_string_to_symbol")
                          ))

(define primitives
  (map (lambda (prim) (car prim)) primitives-labels))

(define code-gen-primitives-closures
  (lambda (f-table)
    (let ((primitives-table (filter (lambda (x) (asm-primitive? (car x))) f-table)))
      (fold-left string-append
                 "// primitives closures\n"
                 (map (lambda (prim) (^primitive-closure prim))
                      primitives-table)))))

(define ^primitive-closure
  (lambda (prim)
    (let* ((var (car prim))
           (add (cadr prim))
           (label (cadr (assoc var primitives-labels))))
      (string-append
       tab "PUSH(3);" nl
       tab "CALL(MALLOC);" nl
       tab "DROP(1);" nl
       tab "MOV(IND(R0), IMM(T_CLOSURE));" nl
       tab "MOV(INDD(R0, 1), IMM(T_NIL));" nl
       tab "MOV(INDD(R0, 2), LABEL(" label "));" nl
       tab "MOV(R1, " (number->string add) ");" nl
       tab "MOV(IND(R1), R0);" nl
       ))))

(define scheme-funcs
  '((define list
      (lambda items items))
    
    (define +
      (letrec ((loop
                (lambda (s)
                  (if (null? s)
                      0
                      (bin+ (car s)
                            (loop (cdr s)))))))
        (lambda s (loop s))))
    
    (define -
      (lambda (a . s)
        (if (null? s)
            (bin- 0 a)
            (bin- a (apply + s)))))
    
    (define *
      (letrec ((loop
                (lambda (s)
                  (if (null? s)
                      1
                      (bin* (car s)
                            (loop (cdr s)))))))
        (lambda s (loop s))))
    
    (define /
      (lambda (a . s)
        (if (null? s)
            (bin/ 1 a)
            (bin/ a (apply * s)))))
    
    (define zero?
      (lambda (x)
        (and (number? x)
             (= x 0))))
    
    (define order
      (lambda (<)
        (letrec ((loop
                  (lambda (first rest)
                    (or (null? rest)
                        (and (< first (car rest))
                             (loop (car rest) (cdr rest)))))))
          (lambda (first . rest)
            (loop first rest)))))
    
    (define bin>? (lambda (a b) (bin<? b a)))
    
    (define < (order bin<?))
    
    (define > (order bin>?))
    
    (define = (order bin=?))
    
    (define not (lambda (x) (if x #f #t)))
    
    (define append
      (letrec ((app2
                (lambda (s1 s2)
                  (if (null? s1) s2
                      (cons (car s1)
                            (app2 (cdr s1) s2)))))
               (appl
                (lambda (s1 s)
                  (if (null? s) s1
                      (app2 s1 (appl (car s) (cdr s)))))))
        (lambda s
          (if (null? s) '()
              (appl (car s) (cdr s))))))
    
    (define map
      ((lambda (y) 
         ((lambda (map1) 
            ((lambda (maplist) 
               (lambda (f . s) 
                 (maplist f s))) 
             (y (lambda (maplist) 
                  (lambda (f s) 
                    (if (null? (car s)) '() 
                        (cons (apply f (map1 car s)) 
                              (maplist f (map1 cdr s))))))))) 
          (y (lambda (map1) 
               (lambda (f s) 
                 (if (null? s) '() 
                     (cons (f (car s)) 
                           (map1 f (cdr s))))))))) 
       (lambda (f) 
         ((lambda (x) 
            (f (lambda (y z)
                 ((x x) y z))))
          (lambda (x) 
            (f (lambda (y z)
                 ((x x) y z))))))))
    
    (define foldr
      (lambda (binop final s)
        (letrec ((loop
                  (lambda (s)
                    (if (null? s) final
                        (binop (car s) (loop (cdr s)))))))
          (loop s))))
    
    (define compose
      (let ((binary-compose
             (lambda (f g)
               (lambda (x)
                 (f (g x))))))
        (lambda s
          (foldr binary-compose (lambda (x) x) s))))
    
    (define caar (compose car car))
    (define cadr (compose car cdr))
    (define cdar (compose cdr car))
    (define cddr (compose cdr cdr))
    (define caaar (compose car caar))
    (define caadr (compose car cadr))
    (define cadar (compose car cdar))
    (define caddr (compose car cddr))
    (define cdaar (compose cdr caar))
    (define cdadr (compose cdr cadr))
    (define cddar (compose cdr cdar))
    (define cdddr (compose cdr cddr))
    (define caaaar (compose car caaar))
    (define caaadr (compose car caadr))
    (define caadar (compose car cadar))
    (define caaddr (compose car caddr))
    (define cadaar (compose car cdaar))
    (define cadadr (compose car cdadr))
    (define caddar (compose car cddar))
    (define cadddr (compose car cdddr))
    (define cdaaar (compose cdr caaar))
    (define cdaadr (compose cdr caadr))
    (define cdadar (compose cdr cadar))
    (define cdaddr (compose cdr caddr))
    (define cddaar (compose cdr cdaar))
    (define cddadr (compose cdr cdadr))
    (define cdddar (compose cdr cddar))
    (define cddddr (compose cdr cdddr))
   
    (define length
      (lambda (s)
        (if (null? s) 0
            (+ (length (cdr s)) 1))))
    
    (define list->vector
      (lambda (s)
        (let* ((n (length s))
               (v (make-vector n)))
          (letrec ((loop
                    (lambda (s i)
                      (if (= i n) v
                          (begin
                            (vector-set! v i (car s))
                            (loop (cdr s) (+ i 1)))))))
            (loop s 0)))))
    
    (define vector (lambda args (list->vector args)))
    ))
