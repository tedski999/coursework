;
; CS1022 Introduction to Computing II 2018/2019
; Lab 2 - Upper Triangular
;

N	EQU	4	
TRUE	EQU	1
FALSE	EQU	0	

	AREA	RESET, CODE, READONLY
	ENTRY

	; initialize system stack pointer (SP)
	LDR	SP, =0x40010000

	;
	; write your program here to determine whether ARR_A
	;   (below) is a matrix in Upper Triangular form.
	;
	; Store 1 in R0 if the matrix is in Upper Triangular form
	;   and zero otherwise.
	;
	
	LDR	R0, =TRUE		;	bool isUT = true;
	LDR	R1, =0			;	int i = 0;
	LDR	R2, =4			;	int j = 4;
	LDR	R3, =N			;	matrixSize = N;
	LSL	R3, R3, #2		;	matrixSize *= 4;
	
wh1	CMP	R2, R3			;	while (j < matrixSize)
	BHS	eWh1			;	{
wh2	CMP	R1, R2			;		while (i < j)
	BHS	eWh2			;		{
	ADD	R4, R1, R2, LSL #2	;			memOffset = i + (j * 4);
	LDR	R4, [R4, #ARR_A]	;			a = loadWord(memOffset + &A);
	CMP	R4, #0			;			if (a != 0)
	BEQ	eIf			;			{
	MOV	R0, #FALSE		;				isUT = false,
	B	eWh1			;				return;
eIf	ADD	R1, R1, #4		;			} i += 4;
	B	wh2			;		}
eWh2	LDR	R1, =0			;		i = 0;
	ADD	R2, R2, #4		;		j += 4;
	B	wh1			;	}
eWh1	

STOP	B	STOP


;
; test data
;

ARR_A	DCD	 1,  2,  3,  4
	DCD	 0,  6,  7,  8
	DCD	 0,  0, 11, 12
	DCD	 0,  0,  0, 16

	END
