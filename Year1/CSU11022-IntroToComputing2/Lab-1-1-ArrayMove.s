;
; CS1022 Introduction to Computing II 2018/2019
; Lab 1 - Array Move
;

; Ted Johnson - 2020/02/13
;
; Use of Registers
; R0 - Array starting address
; R1 - Old index / Current memory location
; R2 - Destination index / Destination memory location
; R3 - Number of elements in array
; R4 - Jumping value
; R5 - Increment direction
; R6 - Copy value

; Tested values (oldIndex, destinationIndex)
; 0,  0
; 0,  1
; 1,  0
; -1, 0
; 0,  -1
; 50, 0
; 0,  50
; 16, 0
; 0, 16
; 6, 3
; 3, 6
; All test cases passed successfully


N	EQU	16

	AREA	globals, DATA, READWRITE

ARRAY	SPACE	N*4


	AREA	RESET, CODE, READONLY
	ENTRY

	LDR	R0, =ARRAY
	LDR	R1, =0
L1	CMP	R1, #N
	BHS	L2
	STR	R1, [R0, R1, LSL #2]
	ADD	R1, R1, #1
	B	L1
L2

	LDR	R0, =ARRAY
	LDR	R1, =3
	LDR	R2, =6
	LDR	R3, =N

					;	// Make sure the indexs are within range of the array first!
					;	// NOTE: No need to check if less than zero, as this is unsigned comparison.
	CMP	R1, R3			;	if (oldIndex >= array.length)
	BHS	STOP			;		return;
	CMP	R2, R3			;	if (destinationIndex >= array.length)
	BHS	STOP			;		return;
					;
					;	// No need to do anything if the indexs are the same!
	CMP	R1, R2			;	if (oldIndex == destinationIndex)
	BEQ	STOP			;		return;
					;
	ADD	R1, R0, R1, LSL #2	;	int currentLocation = arrayStartingLocation + (oldIndex * 4);
	ADD	R2, R0, R2, LSL #2	;	int destinationLocation = arrayStartingLocation + (newIndex * 4);
	CMP	R1, R2			;	if (currentLocation < destinationLocation)
	MOVMI	R5, #4			;		int incrementDirection = 4;
	MOVPL	R5, #-4			;	else	int incrementDirection = -4; 
					;	
	LDR	R4, [R1]		;	int jumpingValue = Memory.getWord(currentLocation);
doWhile					;	do {
	LDR	R6, [R1, R5]		;		int copyValue = Memory.getWord(currentLocation + incrementDirection);
	STR	R6, [R1], R5		;		Memory.setWord(currentLocation, copyValue); currentLocation += incrementDirection;
	CMP	R1, R2			;	}
	BNE	doWhile			;	while (currentLocation != destinationLocation);
	STR	R4, [R2]		;	Memory.setWorld(destinationLocation, jumpingValue);
	
STOP	B	STOP

	END
