;
; CS1022 Introduction to Computing II 2018/2019
; Lab 2 - Matrix Multiplication
;

; Ted Johnson - 2020/02/18

; Register Table
; R2 - arrayRLocation
; R4 - i
; R5 - j
; R6 - k
; R7 - r
; R8 - a
; R9 - b

N	EQU	4		

	AREA	globals, DATA, READWRITE

; result array
ARR_R	SPACE	N*N*4		; N*N words (4 bytes each)


	AREA	RESET, CODE, READONLY
	ENTRY

	; initialize system stack pointer (SP)
	LDR	SP, =0x40010000
	
	
	
	LDR	R2, =ARR_R		;	resultLocation = ARR_R; // Stored in regester because its outside ARM immediate value range
	LDR	R4, =0			;	int i = 0;
wh1	CMP	R4, #(N * 4)		;	while (i < matrixSize)
	BGE	eWh1			;	{
	LDR	R5, =0			;		int j = 0;
wh2	CMP	R5, #(N * 4)		;		while (j < matrixSize)
	BGE	eWh2			;		{
	LDR	R6, =0			;			int k = 0;
	LDR	R7, =0			;			int r = 0;
wh3	CMP	R6, #(N * 4)		;			while (k < matrixSize)
	BGE	eWh3			;			{
	ADD	R8, R4, R6, LSL #2	;				memOffset = i + (k * 4);
	LDR	R8, [R8, #ARR_A]	;				a = Memory.loadWord(memOffset + &A);
	ADD	R9, R6, R5, LSL #2	;				memOffset = k + (j * 4);
	LDR	R9, [R9, #ARR_B]	;				b = Memory.loadWord(memOffset + &B);
	MUL	R8, R9, R8		;				prod = a * b;
	ADD	R7, R7, R8		;				r += prod;
	ADD	R6, R6, #4		;				k += 4;
	B	wh3			;			}
eWh3	ADD	R6, R4, R5, LSL #2	;			memOffset = i + (j * 4);
	STR	R7, [R6, R2]		;			Memory.storeWord(memOffset + resultLocation, r);
	ADD	R5, R5, #4		;			j += 4;
	B	wh2			;		}
eWh2	ADD	R4, R4, #4		;		i += 4;
	B	wh1			;	}
eWh1


STOP	B	STOP


;
; test data
;

ARR_A	DCD	 1,  2,  3,  4
	DCD	 5,  6,  7,  8
	DCD	 9, 10, 11, 12
	DCD	13, 14, 15, 16

ARR_B	DCD	 1,  2,  3,  4
	DCD	 5,  6,  7,  8
	DCD	 9, 10, 11, 12
	DCD	13, 14, 15, 16

	END
