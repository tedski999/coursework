;
; CSU11021 Introduction to Computing I 2019/2020
; Adding the values represented by ASCII digit symbols
;

		AREA	RESET, CODE, READONLY
		ENTRY

		; TEST result = '6' = 0x00000036
		LDR		R1, ='2'		; Load R1 with ASCII code for symbol '2'
		LDR		R2, ='4'		; Load R2 with ASCII code for symbol '4'

		SUB		R1, R1, #0x30	; Convert ASCII to value
		SUB		R2, R2, #0x30	; Convert ASCII to value
		ADD		R0, R1, R2		; Add values together
		ADD		R0, R0, #0x30	; Convert value to ASCII
		
STOP	B	STOP

	END
