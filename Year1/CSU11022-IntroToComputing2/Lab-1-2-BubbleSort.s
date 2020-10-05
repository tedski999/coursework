;
; CS1022 Introduction to Computing II 2019/2020
; Lab 1B - Bubble Sort
;

; Ted Johnson - 2020/02/13
;
; Use of Registers
; R0 - Sorted array start location
; R1 - Sorted array end location
; R2 - Length of array
; R3 - Current location in memory
; R4 - Value in higher index
; R5 - Value in lower index
; R6 - Value has been swapped

; Tested values
; 0,1,2,3,4,5,6,7,8,9
; 1,2,3,4,5,6,7,8,9,0
; 9,3,0,1,6,2,4,7,8,5
; -4,-1,0,2,5,6,8,11,12,-8
; -200,78,0,99,-3,-2,112,200,201,1
; -1,0,1,2,3,0,3,-1,0,1
; All test cases passed successfully


N	EQU	10			; 10 elements in array
TRUE	EQU	1			; TRUE is equal to 1
FALSE	EQU	0			; FALSE is equal to 0

	AREA	globals, DATA, READWRITE

SORTED	SPACE	N*4			; N words (4 bytes each)


	AREA	RESET, CODE, READONLY
	ENTRY

	;
	; Copy the test data into RAM
	;

	LDR	R0, =SORTED
	LDR	R1, =UNSORT
	LDR	R3, =0
whInit	CMP	R3, #N
	BHS	eWhInit
	LDR	R2, [R1, R3, LSL #2]
	STR	R2, [R0, R3, LSL #2]
	ADD	R3, R3, #1
	B	whInit
eWhInit

	;
	; Bubble Sort Algorithm
	;

	LDR	R2, =N			;	int arrayLength = array.length;
	ADD	R1, R0, R2, LSL #2	;	int endLocation = &array + (array.length * 4);
doWhile					;	do {
	LDR	R6, =FALSE		;		bool swapped = false;
	ADD	R3, R0, #4		;		int currentLocation = &array + 4;
while	CMP	R3, R1			;		while (currentLocation < endLocation)
	BGE	eWhile			;		{
	LDR	R4, [R3]		;			int higherValue = Memory.getWord(currentLocation);
	LDR	R5, [R3, #-4]		;			int lowerValue = Memory.getWord(currentLocation - 4);
	CMP	R4, R5			;			if (higherValue < lowerValue)
	BGE	eIf			;			{
	STR	R5, [R3]		;				Memory.setWord(currentLocation, lowerValue);
	STR	R4, [R3, #-4]		;				Memory.setWord(currentLocation - 4, higherValue);
	LDR	R6, =TRUE		;				swapped = true;
eIf	ADD	R3, R3, #4		;			} currentLocation += 4;
	B	while			;		}
eWhile	CMP	R6, #FALSE		;	}
	BNE	doWhile			;	while(swapped);
	
STOP	B	STOP
	

UNSORT	DCD	9,3,0,1,6,2,4,7,8,5

	END
