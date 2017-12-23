/* scheme/make_sob_fraction.asm
 * Takes a fraction, and place the corresponding Scheme object in R0
 * 
 * Programmer: Tal Barami, 2017
 */

 MAKE_SOB_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  PUSH(R0);
  MOV(R1,FPARG(0));
  MOV(R2,FPARG(1));
  PUSH(R1);
  PUSH(R2);
  CALL(GCD);
  POP(R2);
  POP(R1);
  DIV(R2,R0);
  DIV(R1,R0);
  CMP(R2, 0);
  JUMP_GE(SOB_FRACTION_DONE)
  MUL(R2, -1);
  MUL(R1, -1);
  SOB_FRACTION_DONE:
  POP(R0);
  MOV(IND(R0), T_FRACTION);
  MOV(INDD(R0, 1), R1);
  MOV(INDD(R0, 2), R2);
  POP(FP);
  RETURN;
