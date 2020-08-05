;
; CSU11021 Introduction to Computing I 2019/2020
; Convert a sequence of ASCII digits to the value they represent
;

		AREA	RESET, CODE, READONLY
		ENTRY

		; TEST result = '1234' = 0x000007F2
		LDR		R1, ='1'		; Load R1 with ASCII code for symbol '1'
		LDR		R2, ='2'		; Load R2 with ASCII code for symbol '2'
		LDR		R3, ='3'		; Load R3 with ASCII code for symbol '3'
		LDR		R4, ='4'		; Load R4 with ASCII code for symbol '4'
		
		; Convert ASCII to value
		SUB		R1, R1, #0x30
		SUB		R2, R2, #0x30
		SUB		R3, R3, #0x30
		SUB		R4, R4, #0x30
		
		; Convert values to their significant values
		LDR		R0, =1000
		MUL		R1,	R0, R1		; R1 repersents 10^4 significance
		LDR		R0, =100
		MUL		R2,	R0, R2		; R2 repersents 10^3 significance
		LDR		R0, =10
		MUL		R3,	R0, R3		; R3 repersents 10^2 significance
		
		; Add together into the result
		MOV		R0, R1			; R1
		ADD		R0, R0, R2		; R1 + R2
		ADD		R0, R0, R3		; R1 + R2 + R3
		ADD		R0, R0, R4		; R1 + R2 + R3 + R4

STOP	B	STOP

	END
