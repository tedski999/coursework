;
; CSU11021 Introduction to Computing I 2019/2020
; GCD
;

	AREA	RESET, CODE, READONLY
	ENTRY

		; Tested with values (24,32), (12,16), (7,11) and (120,42) 
		LDR		R2, =24		; load test value 24 for a
		LDR		R3, =32		; load test value 32 for b

		; Compute the greatest commmen divisor
tWHILE						;
		CMP		R2, R3		;	while (a != b):	
		BEQ		eWHILE		;
tIF		CMP		R2, R3		;		if (a > b):	
		BLS		tELSE		;
		SUB		R2, R2, R3	;			a = a - b
		B		tWHILE		;		else:
tELSE	SUB		R3, R3, R2	;			b = b - a
		B		tWHILE		;
eWHILE						;

		MOV 	R0, R2		; Move the result into the correct registar

STOP	B	STOP

	END