/* gcd.asm
 * Get the GCD of a given 2 integers: R0 <- GCD(ARG[0], ARG[1])
 *
 * Programmer: Ronen Butirev, 2017
 */

GCD:
  MOV(R1, STARG(0));
  MOV(R2, STARG(1));
  L_GCD_LOOP:
    MOV(R3, R1);
    REM(R3, R2);
	CMP(R3, IMM(0));
	  JUMP_EQ(L_GCD_EXIT);
	MOV(R1, R2);
	MOV(R2, R3);
	JUMP(L_GCD_LOOP);
  L_GCD_EXIT:
	MOV(R0,R2);
	RETURN;