;
; CSU11021 Introduction to Computing I 2019/2020
; Mode
;
; Tested with following inputs:
;  3 and [ 1, 2, 2 ]
;  3 and [ 1, 1, 2 ]
;  8 and [ 5, 3, 7, 5, 3, 5, 1, 9 ]
;  8 and [ 1, 1, 1, 1, 1, 1, 1, 1 ]
;  1 and [ 1 ]
;  1 and [ 0 ]
;
; Important Variables Table
;  R0 - mode
;  R1 - setLength
;  R2 - setStartAddr
;  R3 - setEndAddr
;  R4 - curItemAddr
;  R5 - curItem
;  R6 - cmpItemAddr
;  R7 - cmpItem
;  R8 - curCount
;  R9 - recordCount
;

	AREA	RESET, CODE, READONLY
	ENTRY

	LDR	R1, =tstN	;	setLengthAddr = GetAddr('tstN');
	LDR	R1, [R1]	;	setLength = LoadWord(setLengthAddr);
	LDR	R2, =tstvals	;	setStartAddr = GetAddr('tstvals');
	MOV	R3, R1, LSL #2	;	setByteLength = setLength * 4;
	ADD	R3, R2, R3	;	setEndAddr = setStartArr + setByteLength
	MOV	R4, R2		;	curItemAddr = setStartAddr;
	LDR	R9, =0		;	recordCount = 0;
				;
outerL				;	do {
	LDR	R8, =0		;		curCount = 0;
	LDR	R5, [R4]	;		curItem = LoadWord(curItemAddr);
	MOV	R6, R2		;		cmpItemAddr = setAddr;
				;
innerL				;		do {
	LDR	R7, [R6]	;			cmpItem = LoadWord(cmpItemAddr);
	CMP	R7, R5		;			if (cmpItem == curItem)
	ADDEQ	R8, R8, #1	;				curCount += 1;
	ADD	R6, R6, #4	;			cmpItemAddr += 4;
	CMP	R6, R3		;		}
	BNE	innerL		;		while (cmpItemAddr != setEndAddr);
				;
	CMP	R8, R9		;		if (curCount > recordCount)
	BLS	eIf		;		{
	MOV	R9, R8		;			recordCount = curCount;
	MOV	R0, R5		;			mode = curItem;
eIf				;		}
	ADD	R4, R4, #4	;		curItemAddr += 4;
	CMP	R4, R3		;	}
	BNE	outerL		;	while (curItemAddr != setEndAddr);

STOP	B	STOP

tstN	DCD	8			; N (number of numbers)
tstvals	DCD	5, 3, 7, 5, 3, 5, 1, 9	; numbers

	END
