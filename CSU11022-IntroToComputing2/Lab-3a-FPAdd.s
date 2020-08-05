;
; CS1022 Introduction to Computing II 2018/2019
; Lab 3 - Floating-Point
;
; Ted Johnson - 2020/03/25

	AREA	RESET, CODE, READONLY
	ENTRY

;
; Test Data
;
FP_A	EQU	0x41C40000
FP_B	EQU	0x41960000


	; initialize system stack pointer (SP)
	LDR	SP, =0x40010000
	
	
	; TESTING WITH FP_A

	; Decoded fraction from FP_A
	LDR	R0, =FP_A		;	int decodedFraction = fpfrac(FP_A);
	BL	fpfrac			;	 ^
	MOV	R4, R0			;	 ^
	; The value in R4 (decodedFraction) should now be equal to the fraction of FP_A
	
	; Decode exponent from FP_A
	LDR	R0, =FP_A		;	int decodedExponent = fpexp(FP_A);
	BL	fpexp			;	 ^
	MOV	R5, R0			;	 ^
	; The value in R5 (decodedExponent) should now be equal to the exponent of FP_A
	
	; Encode decoded fraction and exponent
	MOV	R0, R4			;	float encodedFloat = fpencode(decodedFtaction, decodededExponent);
	MOV	R1, R5			;	 ^
	BL	fpencode		;	 ^
	MOV	R6, R0			;	 ^
	; The value in R6 (encodedFloat) should now be equal to the value of FP_A
	
	
	; TESTING WITH FP_B
	
	; Decoded fraction from FP_B
	LDR	R0, =FP_B		;	int decodedFraction = fpfrac(FP_B);
	BL	fpfrac			;	 ^
	MOV	R7, R0			;	 ^
	; The value in R7 (decodedFraction) should now be equal to the fraction of FP_B
	
	; Decode exponent from FP_A
	LDR	R0, =FP_B		;	int decodedExponent = fpexp(FP_B);
	BL	fpexp			;	 ^
	MOV	R8, R0			;	 ^
	; The value in R8 (decodedExponent) should now be equal to the exponent of FP_B
	
	; Encode decoded fraction and exponent
	MOV	R0, R7			;	float encodedFloat = fpencode(decodedFtaction, decodededExponent);
	MOV	R1, R8			;	 ^
	BL	fpencode		;	 ^
	MOV	R9, R0			;	 ^
	; The value in R9 (encodedFloat) should now be equal to the value of FP_B
	

stop	B	stop


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
	
	MOV	R4, R0			;	Move parameter 0 into local scope
	LDR	R5, =0x807FFFFF		;	Save bits 0-22 (the value) and 31 (the sign)
	AND	R0, R4, R5		;	 ^
	
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
	
	MOV	R4, R0			;	Move parameter 0 into local scope
	LDR	R5, =0x7F800000		;	Save bits 23-30
	AND	R4, R4, R5		;	 ^
	LSR	R4, R4, #23		;	Shift value by 23 bits right
	SUB	R0, R4, #127		;	Subract bias of 127
	
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
	
	MOV	R4, R0			;	Move parameter 0 into local scope
	MOV	R5, R1			;	Move parameter 1 into local scope
	ADD	R5, R5, #127		;	Add bias of 127 to exponent
	LSL	R5, R5, #23		;	Shift exponent by 23 bits left
	ORR	R0, R4, R5		;	Merge the fraction and the exponent (the sign is contained within the fraction)
	
	POP	{ R4-R5, PC }


	END
