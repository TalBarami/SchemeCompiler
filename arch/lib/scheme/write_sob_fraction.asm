/* scheme/write_sob_fraction.asm
 * Take a pointer to a Scheme fraction object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 * Programmer: Tal Barami, 2017
 */

 WRITE_SOB_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  MOV(R0, FPARG(0));
  MOV(R1, INDD(R0, 2));
  PUSH(R1);
  PUSH(IMM('/'));
  MOV(R1, INDD(R0, 1));
  PUSH(R1);
  CALL(WRITE_INTEGER);
  DROP(1);
  CALL(PUTCHAR);
  DROP(1);
  CALL(WRITE_INTEGER);
  DROP(1);
  POP(R1);
  
  POP(FP);
  RETURN;

