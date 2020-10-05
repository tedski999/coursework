;
; CS1022 Introduction to Computing II 2018/2019
; Magic Square
;
; Ted Johnson - 03/05/2020
;

TRUE	EQU	1
FALSE	EQU	0

	AREA	RESET, CODE, READONLY
	ENTRY

	; initialize system stack pointer (SP)
	LDR	SP, =0x40010000

	; ==================================
	; === Testing isMagic Subroutine ===

	; Test isMagic subroutine with arr1
	LDR	R0, =arr1	;	param0 = arr1;
	LDR	R1, =size1	;	size = Memory.loadWord(size1);
	LDR	R1, [R1]	;	param1 = size;
	BL	isMagic		;	isMagic(param0, param1);
	; R0 should be 1 as arr1 is magic
	
	; Test isMagic subroutine with arr2
	LDR	R0, =arr2	;	param0 = arr2;
	LDR	R1, =size2	;	size = Memory.loadWord(size2);
	LDR	R1, [R1]	;	param1 = size;
	BL	isMagic		;	isMagic(param0, param1);
	; R0 should be 0 as arr2 is not magic
	
	; Test isMagic subroutine with arr3
	LDR	R0, =arr3	;	param0 = arr3;
	LDR	R1, =size3	;	size = Memory.loadWord(size3);
	LDR	R1, [R1]	;	param1 = size;
	BL	isMagic		;	isMagic(param0, param1);
	; R0 should be 1 as arr3 is magic
	
	; Test isMagic subroutine with arr4
	LDR	R0, =arr4	;	param0 = arr4;
	LDR	R1, =size4	;	size = Memory.loadWord(size4);
	LDR	R1, [R1]	;	param1 = size;
	BL	isMagic		;	isMagic(param0, param1);
	; R0 should be 0 as arr4 is not magic
	
	; Test isMagic subroutine with arr5
	LDR	R0, =arr5	;	param0 = arr5;
	LDR	R1, =size5	;	size = Memory.loadWord(size5);
	LDR	R1, [R1]	;	param1 = size;
	BL	isMagic		;	isMagic(param0, param1);
	; R0 should be 1 as arr5 is trivial
	
	; Test isMagic subroutine with arr6
	LDR	R0, =arr6	;	param0 = arr6;
	LDR	R1, =size6	;	size = Memory.loadWord(size6);
	LDR	R1, [R1]	;	param1 = size;
	BL	isMagic		;	isMagic(param0, param1);
	; R0 should be 0 as arr6 is not magic

stop	B	stop

;
; isMagic
; Determines whether a square two-dimensional array in memory is a Magic Square
; A Magic Square must have a width of at least 3, else the suybroutine will return FALSE
; parameters:
;	r0 - 2D square array location
;	r1 - 2D square array width
; result:
;	r0 - Returns TRUE if the array is a Magic Square, else returns FALSE
;
isMagic
	PUSH	{ R4-R12, LR }			;	push on local stack
	MOV	R4, R0				;	int curAddr = param0;
	MOV	R5, R1				;	int arrSize = param1;
	
	; Check for trivial/invalid parameters
	CMP	R5, #2				;	if (arrSize <= 2)
	BGT	isMagic_notTrivial		;	{
	CMP	R5, #1				;		
	MOVEQ	R6, #TRUE			;		if (arrSize == 1) result = true;
	MOVNE	R6, #FALSE			;		else              result = false;
	B	isMagic_end			;		goto isMagic_end;
isMagic_notTrivial				;	}
	
	; ===================================
	; === Check the primary diagonals ===
	
	; Sum of leading diagonal (0, 4, 8) ... in steps of arrSize+1 (+4)
	LDR	R7, [R4]			;	int leadingDiagonalSum = Memory.loadWord(curAddr);
	ADD	R9, R5, #1			;	int step = arrSize + 1;
	LDR	R10, =1				;	int index = 1;
isMagic_leadingDiagonalLoop			;	do {
	LDR	R11, [R4, R9, LSL #2]!		;		curAddr += step * 4; int value = Memory.loadWord(curAddr);
	ADD	R7, R7, R11			;		leadingDiagonalSum += value;
	ADD	R10, R10, #1			;		index++;
	CMP	R10, R5				;	}
	BLT	isMagic_leadingDiagonalLoop	;	while (index < arrSize);
	
	; Sum of counter diagonal (6, 4, 2) ... in steps of 1-arrSize (-2)
	LDR	R8, =0				;	int counterDiagonalSum = 0;
	RSB	R9, R5, #1			;	step = 1 - arrSize;
	ADD	R4, R4, R9, LSL #2		;	curAddr += step * 4;
isMagic_counterDiagonalLoop			;	do {
	LDR	R11, [R4], R9, LSL #2		;		int value = Memory.loadWord(curAddr); curAddr += step * 4;
	ADD	R8, R8, R11			;		leadingDiagonalSum += value;
	SUB	R10, R10, #1			;		index--;
	CMP	R10, #0				;	}
	BGT	isMagic_counterDiagonalLoop	;	while (index > 0);
	
	; Check if they're equal
	CMP	R7, R8				;	if (leadingDiagonalSum != counterDiagonalSum)
	BNE	isMagic_end			;		goto isMagic_end;
	

	; =================================
	; === Check the horizontal rows ===
	
	LDR	R9, =0				;	int index = 0;
isMagic_horizontalLoopRows			;	do {
	LDR	R10, =1				;		int sumCount = 1;
	LDR	R8, [R4]			;		int sum = Memory.loadWord(curAddr);
isMagic_horizontalLoopColumns			;		do {
	LDR	R11, [R4, #4]!			;			curAddr += 4; int value = Memory.loadWord(curAddr);
	ADD	R8, R8, R11			;			sum += value;
	ADD	R10, R10, #1			;			sumCount++;
	CMP	R10, R5				;		}
	BLT	isMagic_horizontalLoopColumns	;		while (sumCount < arrSize);
	CMP	R7, R8				;		if (magicNumber != sum)
	BNE	isMagic_end			;			goto isMagic_end;
	ADD	R4, R4, #4			;		curAddr += 4;
	ADD	R9, R9, #1			;		index++;
	CMP	R9, R5				;	}
	BLT	isMagic_horizontalLoopRows	;	while (index < arrSize);
	

	; ==================================
	; === Check the vertical columns ===
	
	LDR	R9, =0				;	int index = 0;
	SUB	R12, R4, #4			;	int baseAddr = curAddr - 4;
isMagic_verticalLoopColumns			;	do {
	SUB	R4, R12, R9, LSL #2		;		curAddr = baseAddr - (index * 4);
	LDR	R10, =1				;		int sumCount = 1;
	LDR	R8, [R4]			;		int sum = Memory.loadWord(curAddr);
isMagic_verticalLoopRows			;		do {
	SUB	R4, R4, R5, LSL #2		;			curAddr -= arrSize * 4;
	LDR	R11, [R4]			;			int value = Memory.loadWord(curAddr);
	ADD	R8, R8, R11			;			sum += value;
	ADD	R10, R10, #1			;			sumCount++;
	CMP	R10, R5				;		}
	BLT	isMagic_verticalLoopRows	;		while (sumCount < arrSize);
	CMP	R7, R8				;		if (magicNumber != sum)
	BNE	isMagic_end			;			goto isMagic_end;
	ADD	R9, R9, #1			;		index++;
	CMP	R9, R5				;	}
	BLT	isMagic_verticalLoopColumns	;	while (index < arrSize);
	

	; ==============================
	; === Return with the result ===
	
	; All checks passed, return true
	LDR	R6, =TRUE			;	result = true;
	
	; Common exit
isMagic_end
	MOV	R0, R6				;	return result;
	POP	{ R4-R12, PC }			;	pop off local stack


; Test data 1 (3x3, magic)
size1	DCD	3
arr1	DCD	2,7,6
	DCD	9,5,1
	DCD	4,3,8

; Test data 2 (3x3, not magic)
size2	DCD	3
arr2	DCD	1,2,3
	DCD	4,5,6
	DCD	7,8,9
		
; Test data 3 (4x4, magic)
size3	DCD	4
arr3	DCD	16,03,02,13
	DCD	05,10,11,08
	DCD	09,06,07,12
	DCD	04,15,14,01
		
; Test data 4 (4x4, not magic)
size4	DCD	4
arr4	DCD	01,02,03,04
	DCD	05,06,07,08
	DCD	09,10,11,12
	DCD	13,14,15,16
		
; Test data 5 (1x1, trivial)
size5	DCD	1
arr5	DCD	9
	
; Test data 6 (2x2, not magic)
size6	DCD	2
arr6	DCD	9,9
	DCD	9,9

	END
