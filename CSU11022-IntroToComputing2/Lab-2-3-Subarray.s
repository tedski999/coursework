;
; CS1022 Introduction to Computing II 2018/2019
; Lab 2 - Subarray
;

N	EQU	7
M	EQU	3
TRUE	EQU	1
FALSE	EQU	0	

	AREA	RESET, CODE, READONLY
	ENTRY

	; initialize system stack pointer (SP)
	LDR	SP, =0x40010000

	;
	; Write your program here to determine whether SMALL_A
	;   is a subarray of LARGE_A
	;
	; Store 1 in R0 if SMALL_A is a subarray and zero otherwise
	;

	LDR	R9, =N		;	int matNSize = N;
	LDR	R10, =M		;	int matMSize = M;
	LDR	R11, =LARGE_A	;	int matNLocation = LARGE_A;
	LDR	R12, =SMALL_A	;	int matMLocation = SMALL_A;

	LDR	R0, =FALSE	;	bool isSubArray = false;
	LDR	R1, =0		;	int matN_i = 0;
	LDR	R2, =0		;	int matN_j = 0;
	
whNj	CMP	R2, #(N - M)	;	while (matN_j <= (N-M))
	BGT	eWhNj		;	{
whNi	CMP	R1, #(N - M)	;	whNi:	while (matN_i <= (N-M))
	BGT	eWhNi		;		{
				;			
	LDR	R3, =0		;			int matM_i = 0;
	LDR	R4, =0		;			int matM_j = 0;
				;			
whMj	CMP	R4, #M		;			while (matM_j < M)
	BGE	eWhMj		;			{
whMi	CMP	R3, #M		;				while (matM_i < M)
	BGE	eWhMi		;				{
				;					
	MUL	R7, R10, R4	;					int temp = matMSize * matM_j;
	ADD	R7, R7, R3	;					temp += matM_i;
	LSL	R7, R7, #2	;					temp *= 4;
	LDR	R5, [R7, R12]	;					int m = loadWord(temp);
				;					
	ADD	R7, R2, R4	;					int temp = matN_j + matM_j;
	MUL	R7, R9, R7	;					temp *= matNSize;
	ADD	R7, R7, R1	;					temp += matN_i;
	ADD	R7, R7, R3	;					temp += matM_i;
	LSL	R7, R7, #2	;					temp *= 4;
	LDR	R6, [R7, R11]	;					int n = loadWord(temp);
				;					
	CMP	R5, R6		;					if (m != n)
	BEQ	equal		;					{
	ADD	R1, R1, #1	;						matN_i++,
	B	whNi		;						goto whNi;
equal				;					}
				;					
	ADD	R3, R3, #1	;					matM_i++;
	B	whMi		;				}
eWhMi	LDR	R3, =0		;				matM_i = 0;
	ADD	R4, R4, #1	;				matM_j++;
	B	whMj		;			}
				;			
eWhMj	LDR	R0, =TRUE	;			isSubArray = true;	
	B	STOP		;			return;
	B	whNi		;		}
eWhNi	LDR	R1, =0		;		matM_i = 0;
	ADD	R2, R2, #1	;		matM_j++;
	B	whNj		;	}
eWhNj				;	


STOP	B	STOP


;
; test data
;

LARGE_A	DCD	 48, 37, 15, 44,  3, 17, 26
	DCD	  2,  9, 12, 18, 14, 33, 16
	DCD	 13, 20,  1, 22,  7, 48, 21
	DCD	 27, 19, 44, 48, 44, 18, 10
	DCD	 29, 17, 22,  4, 46, 43, 41
	DCD	 37, 35, 38, 34, 16, 25,  0
	DCD	 17,  0, 48, 15, 27, 35, 11

SMALL_A	DCD	 49, 44, 18
	DCD	  4, 46, 43
	DCD	 34, 16, 25

	END
