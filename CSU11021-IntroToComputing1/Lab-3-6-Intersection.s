;
; CSU11021 Introduction to Computing I 2019/2020
; Intersection
;
; Tested with following inputs:
;  2 and [ 1, 2 ] and 2 and [ 2, 3 ]
;  2 and [ 1, 2 ] and 2 and [ 1, 2 ]
;  2 and [ 1, 2 ] and 2 and [ 3, 4 ]
;  2 and [ 3, 2 ] and 2 and [ 2, 1 ]
;  4 and [ 7, 14, 6, 3 ] and 9 and [ 20, 11, 14, 5, 7, 2, 9, 12, 17 ]
;  1 and [ 1 ] and 3 and [ 1, 2, 3 ]
;
; Important Variables Table
;  R0 - sizeC_Addr
;  R1 - curC_Addr
;  R2 - curA_Addr
;  R3 - endA_Addr
;  R4 - curB_Addr
;  R5 - endB_Addr
;  R6 - sizeC
;  R7 - elemA
;  R8 - elemB

	AREA	RESET, CODE, READONLY
	ENTRY

	LDR	R0, =0x40000000	;	sizeC_Addr = 0x40000000;
	LDR	R1, =0x40000004	;	curC_Addr = 0x40000004;
				;
	LDR	R2, =elemsA	;	curA_Addr = GetAddr(elemsA);
	LDR	R3, =sizeA	;	sizeA_Addr = GetAddr('sizeA');
	LDR	R3, [R3]	;	sizeA = LoadWord(sizeA_Addr);
	MOV	R3, R3, LSL #2	;	bytesizeA = sizeA * 4;
	ADD	R3, R2, R3	;	endA_Addr = curA_Addr + bytesizeA
				;
	LDR	R4, =elemsB	;	curB_Addr = GetAddr(elemsB);
	LDR	R5, =sizeB	;	sizeB_Addr = GetAddr('sizeB');
	LDR	R5, [R5]	;	sizeB = LoadWord(sizeB_Addr);
	MOV	R5, R5, LSL #2	;	bytesizeB = sizeB * 4;
	ADD	R5, R4, R5	;	endB_Addr = curB_Addr + bytesizeB
				;
	LDR	R8, =0		;	sizeC = 0;
				;
outerL	CMP	R2, R3		;	while (curA_Addr != endA_Addr)
	BEQ	eOuter		;	{
	LDR	R7, [R2]	;		elemA = LoadWord(curA_Addr);
	LDR	R4, =elemsB	;		curB_Addr = GetAddr(elemsB);
innerL	CMP	R4, R5		;		while (curB_Addr != endB_Addr)
	BEQ	eInner		;		{
	LDR	R8, [R4]	;			elemB = LoadWord(curB_Addr);
	CMP	R7, R8		;			if (elemA == elemB)
	BNE	eIf		;			{
	STR	R7, [R1]	;				StoreWord(curC_Addr, elemA);
	ADD	R1, R1, #4	;				curC_Addr += 4;
	ADD	R6, R6, #1	;				sizeC += 1;
	B	eInner		;				break;
eIf				;			}
	ADD	R4, R4, #4	;			curB_Addr += 4;
	B	innerL		;		}
eInner	ADD	R2, R2, #4	;		curA_Addr += 4;
	B	outerL		;	}
eOuter	STR	R6, [R0]	;	StoreWord(sizeC_Addr, sizeC);

STOP	B	STOP

sizeA	DCD	4
elemsA	DCD	7, 14, 6, 3

sizeB	DCD	9
elemsB	DCD	20, 11, 14, 5, 7, 2, 9, 12, 17

	END
