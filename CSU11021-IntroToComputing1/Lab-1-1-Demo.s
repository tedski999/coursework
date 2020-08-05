;
; CSU11021 Introduction to Computing I 2019/2020
; Simple ARM Assembly Language Demonstration
;

	AREA	RESET, CODE, READONLY
	ENTRY

	MOV	R0, R1
	ADD	R0, R0, R2
	ADD	R0, R0, R3
	ADD	R0, R0, R4

STOP	B	STOP

	END
