/* scheme/make_sob_symbol.asm
 * Takes an integer as an argument, and places 
 * in R0 the corresponding representitive string address
 * of the symbol object.
 * 
 * Programmers: Tal Barami, Ronen Butirev, 2017
 */

 MAKE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(2));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_SYMBOL);
  MOV(INDD(R0, 1), FPARG(0));
  POP(FP);
  RETURN;

