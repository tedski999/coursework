;
; CS1022 Introduction to Computing II 2018/2019
; Lab 3 - Floating-Point
;
; Ted Johnson - 2020/03/27

;
; Test Data
;
FP_A	EQU	0x41C40000
FP_B	EQU	0x41960000

	AREA	RESET, CODE, READONLY
	ENTRY

	; initialize system stack pointer (SP)
	LDR	SP, =0x40010000
	
	; Add FP_A and FP_B
	LDR	R0, =FP_A	;	param1 = FP_A;
	LDR	R1, =FP_B	;	param2 = FP_B;
	BL	fpadd		;	result = fpadd(param1, param2);

stop	B	stop

; fpadd
; adds two IEEE 754 floating point values together
; parameters:
;	r0 - ieee 754 float
;	r1 - ieee 754 float
; return:
;	r0 - ieee 754 float
fpadd
	PUSH	{ R4-R11, LR }
	
	; Move the parameters into local scope
	MOV	R4, R0		;	int fp1 =  param1;
	MOV	R5, R1		;	int fp2 =  param2;
	
	; Get the fractions of each float
	MOV	R0, R4		;	param1 = fp1;
	BL	fpfrac		;	result = fpfrac(param1);
	MOV	R6, R0		;	int fp1_frac = result;
	MOV	R0, R5		;	param1 = fp2;
	BL	fpfrac		;	result = fpfrac(param1);
	MOV	R7, R0		;	int fp2_frac = result;
	
	; Get the exponents of each float
	MOV	R0, R4		;	param1 = fp1;
	BL	fpexp		;	result = fpexp(param1);
	MOV	R8, R0		;	int fp1_exp = result;
	MOV	R0, R5		;	param1 = fp2;
	BL	fpexp		;	result = fpexp(param1);
	MOV	R9, R0		;	int fp2_exp = result;
	
	; Shift the second float so both exponents match
fpadd_whGT			;
	CMP	R8, R9		;	while (fp1_exp < fp2_exp)
	BGE	fpadd_whLT	;	{
	ADD	R9, R9, #1	;		fp2_exp++;
	LSL	R7, R7, #1	; 		fp2_frac = fp2_frac << 1;
	B	fpadd_whGT	;	}
fpadd_whLT			;	
	CMP	R8, R9		;	while (fp1_exp > fp2_exp)
	BLE	fpadd_eWh	;	{
	SUB	R9, R9, #1	;		fp2_exp--;
	LSR	R7, R7, #1	; 		fp2_frac = fp2_frac >> 1;
	B	fpadd_whLT	;	}
fpadd_eWh			;
	
	; Add the two float fractions together
	ADD	R10, R6, R7	;	int fp_frac = fp1_frac + fp2_frac;
	LSR	R10, R10, #1	;	fp_frac = fp_frac >> 1;
	ADD	R11, R8, #1	;	int fp_exp = fp1_exp + 1;
	
	; Encode the new float
	MOV	R0, R10		;	param1 = fp_frac;
	MOV	R1, R11		;	param2 = fp_exp;
	BL	fpencode	;	result = fpencode(param1, param2);
	
	; Pop off the local scope and return the result
	POP	{ R4-R11, PC }


;
; fpfrac
; decodes an IEEE 754 floating point value to the signed (2's complement)
; fraction
; parameters:
;	r0 - ieee 754 float
; return:
;	r0 - fraction (signed 2's complement word)
;
fpfrac
	PUSH	{ R4-R5, LR }
	
	MOV	R4, R0			;	int fp = param1
	LDR	R5, =0x807FFFFF		;	int mask = 0x7FFFFF;
	AND	R4, R4, R5		;	int frac = fp & mask; // Save bits 0-22
	ORR	R0, R4, #0x800000	;	frac = frac | 0x800000; // Restore hidden bit
	
	; Pop off the local scope and return the result
	POP	{ R4-R5, PC }


;
; fpexp
; decodes an IEEE 754 floating point value to the signed (2's complement)
; exponent
; parameters:
;	r0 - ieee 754 float
; return:
;	r0 - exponent (signed 2's complement word)
;
fpexp
	PUSH	{ R4-R5, LR }
	
	MOV	R4, R0			;	int fp = param1;
	LDR	R5, =0x7F800000		;	int mask = 0x7F800000;
	AND	R4, R4, R5		;	int exp = fp & mask; // Save bits 23-30
	LSR	R4, R4, #23		;	exp = exp >> 23;
	SUB	R0, R4, #127		;	result = exp - 127;
	
	; Pop off the local scope and return the result
	POP	{ R4-R5, PC }


;
; fpencode
; encodes an IEEE 754 value using a specified fraction and exponent
; parameters:
;	r0 - fraction (signed 2's complement word)
;	r1 - exponent (signed 2's complement word)
; result:
;	r0 - ieee 754 float
;
fpencode
	PUSH	{ R4-R5, LR }
	
	MOV	R4, R0			;	int frac = param1;
	MOV	R5, R1			;	int exp = param2;
	BIC	R0, R4, #0x800000	;	frac = frac & 0x7FFFFF; // Clear hidden bit
	ADD	R5, R5, #127		;	exp += 127;
	LSL	R5, R5, #23		;	exp = exp << 23;
	ORR	R0, R4, R5		;	result = frac | exp; // Merge the fraction and the exponent
	
	; Pop off the local scope and return the result
	POP	{ R4-R5, PC }


	END