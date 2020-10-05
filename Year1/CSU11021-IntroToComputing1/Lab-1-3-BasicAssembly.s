;
; CSU11021 Introduction to Computing I 2019/2020
; Basic ARM Assembly Language
;

		AREA	RESET, CODE, READONLY
		ENTRY

; (i) 3x + y

		; TEST result = 9 = 0x00000009
		LDR		R1, =2		; x = 2
		LDR		R2, =3		; y = 3
		
		LDR		R3, =3		; temp = 3
		MUL		R0, R1, R3	; result = 3x
		ADD		R0, R2		; result = 3x + y

; (ii) 3x^2 + 5x

		; TEST result = 22 = 0x00000016
		LDR		R1, =2		; x = 2
		
		; TERM 3x^2
		MUL		R0, R1, R1	; result = x^2
		LDR		R2, =3		; temp = 3
		MUL		R0, R2, R0	; result = 3x^2
		
		; TERM 5x
		LDR		R2, =5		; temp = 5
		MUL		R2, R1, R2	; temp = 5x
		ADD		R0, R0, R2	; result = 3x^2 + 5x

; (iii) 2x^2 + 6xy + 3y^2

		; TEST result = 71 = 0x00000047
		LDR		R1, =2		; x = 2
		LDR		R2, =3		; y = 3
		
		; TERM 2x^2
		LDR		R3, =2		; temp = 2
		MUL		R0, R1, R1	; result = x^2
		MUL		R0, R3, R0	; result = 2x^2
		
		; TERM 6xy
		LDR		R3, =6		; temp = 6
		MUL		R3, R1, R3	; temp = 6x
		MUL		R3, R2, R3	; temp = 6xy
		ADD		R0, R0, R3	; result = 2x^2 + 6xy
		
		; TERM 3y^2
		LDR		R1, =3		; temp = 3
		MUL		R3, R2, R2	; x = y^2
		MUL		R3, R1, R3	; temp = 3y^2
		ADD		R0, R0, R3	; result = 2x^2 + 6xy + 3y^2

; (iv) x^3 - 4x^2 + 3x + 8

		; TEST: result = 6 = 0x00000006
		LDR		R1, =2		; x = 2
		LDR		R0, =8		; result = 8
		
		; TERM 3x
		LDR		R2, =3		; temp = 3
		MUL		R2, R1, R2	; temp = 3x
		ADD		R0, R0, R2	; result = 3x + 8
		
		; TERM 4x^2
		MUL		R3, R1, R1	; x^2 stored in R2 for later use
		LDR		R2, =4		; temp = 4
		MUL		R2, R3, R2	; temp = 4x^2
		SUB		R0, R0, R2	; result = -4x^2 + 3x + 8
		
		; TERM x^3
		MUL		R2, R1, R3	; temp = X^3
		ADD		R0, R0, R2	; result = x^3 - 4x^2 + 3x + 8

STOP	B	STOP

	END
