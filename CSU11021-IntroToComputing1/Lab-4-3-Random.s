;
; CSU11021 Introduction to Computing I 2019/2020
; Pseudo-random number generator
;
; Implementation of Middle Square with Weyl Sequence PRNG
; Similar to GLSL's rand() function

	AREA	RESET, CODE, READONLY
	ENTRY

	LDR	R0, =0x40000000	; start address for pseudo-random sequence
	LDR	R1, =64		; number of pseudo-random values to generate
	LDR	R2, =4		; x ... also PRNG seed
	LDR	R3, =0		; difference sequence
	LDR	R4, =0xb5ad4ece	; weyl sequence

loop
	MUL	R5, R2, R2	; temp1 = x * x
	ADD	R3, R3, R4	; w = w + s
	ADD	R2, R3, R5	; x = w + temp1
	MOV	R5, R2, LSR #8	; temp1 = x >> 8
	MOV	R6, R2, LSL #8	; temp2 = x << 8
	ORR	R2, R5, R6	; x = temp1 | temp2
	STR	R2, [R0]	; store x to memory
	ADD	R0, R0, #4	; memory += 4
	SUB	R1, R1, #1	; remaining -= 1
	CMP	R1, #0		; if remaining != 0:
	BNE	loop		;	branch to top

STOP	B	STOP

	END
